#include "btpd.h"

#include <ctype.h>

struct meta_peer *
mp_create(void)
{
    return btpd_calloc(1, sizeof(struct meta_peer));
}

void
mp_kill(struct meta_peer *mp)
{
    free(mp);
}

void
mp_hold(struct meta_peer *mp)
{
    mp->refs++;
}

void
mp_drop(struct meta_peer *mp, struct net *n)
{
    assert(mp->refs > 0);
    mp->refs--;
    if (mp->refs == 0) {
        assert(mp->p == NULL);
        if (mp->flags & PF_ATTACHED)
            assert(mptbl_remove(n->mptbl, mp->id) == mp);
        mp_kill(mp);
    }
}

void
peer_kill(struct peer *p)
{
    struct nb_link *nl;

    btpd_log(BTPD_L_CONN, "killed peer %p\n", p);

    if (p->mp->flags & PF_ATTACHED) {
        BTPDQ_REMOVE(&p->n->peers, p, p_entry);
        p->n->npeers--;
        if (p->n->active) {
            ul_on_lost_peer(p);
            dl_on_lost_peer(p);
        }
    } else
        BTPDQ_REMOVE(&net_unattached, p, p_entry);
    if (p->mp->flags & PF_ON_READQ)
        BTPDQ_REMOVE(&net_bw_readq, p, rq_entry);
    if (p->mp->flags & PF_ON_WRITEQ)
        BTPDQ_REMOVE(&net_bw_writeq, p, wq_entry);

    btpd_ev_del(&p->ioev);
    close(p->sd);

    nl = BTPDQ_FIRST(&p->outq);
    while (nl != NULL) {
        struct nb_link *next = BTPDQ_NEXT(nl, entry);
        nb_drop(nl->nb);
        free(nl);
        nl = next;
    }

    p->mp->p = NULL;
    mp_drop(p->mp, p->n);
    if (p->in.buf != NULL)
        free(p->in.buf);
    if (p->piece_field != NULL)
        free(p->piece_field);
    if (p->bad_field != NULL)
        free(p->bad_field);
    free(p);
    net_npeers--;
}

void
peer_set_in_state(struct peer *p, enum input_state state, size_t size)
{
    p->in.state = state;
    p->in.st_bytes = size;
}

void
peer_send(struct peer *p, struct net_buf *nb)
{
    struct nb_link *nl = btpd_calloc(1, sizeof(*nl));
    nl->nb = nb;
    nb_hold(nb);

    if (BTPDQ_EMPTY(&p->outq)) {
        assert(p->outq_off == 0);
        btpd_ev_enable(&p->ioev, EV_WRITE);
        p->t_wantwrite = btpd_seconds;
    }
    BTPDQ_INSERT_TAIL(&p->outq, nl, entry);
}

/*
 * Remove a network buffer from the peer's outq.
 * If a part of the buffer already have been written
 * to the network it cannot be removed.
 *
 * Returns 1 if the buffer is removed, 0 if not.
 */
int
peer_unsend(struct peer *p, struct nb_link *nl)
{
    if (!(nl == BTPDQ_FIRST(&p->outq) && p->outq_off > 0)) {
        BTPDQ_REMOVE(&p->outq, nl, entry);
        if (nl->nb->type == NB_TORRENTDATA) {
            assert(p->npiece_msgs > 0);
            p->npiece_msgs--;
        }
        nb_drop(nl->nb);
        free(nl);
        if (BTPDQ_EMPTY(&p->outq)) {
            if (p->mp->flags & PF_ON_WRITEQ) {
                BTPDQ_REMOVE(&net_bw_writeq, p, wq_entry);
                p->mp->flags &= ~PF_ON_WRITEQ;
            } else
                btpd_ev_disable(&p->ioev, EV_WRITE);
        }
        return 1;
    } else
        return 0;
}

void
peer_sent(struct peer *p, struct net_buf *nb)
{
    switch (nb->type) {
    case NB_KEEPALIVE:
        btpd_log(BTPD_L_MSG, "sent keepalive to %p\n", p);
        break;
    case NB_CHOKE:
        btpd_log(BTPD_L_MSG, "sent choke to %p\n", p);
        break;
    case NB_UNCHOKE:
        btpd_log(BTPD_L_MSG, "sent unchoke to %p\n", p);
        p->mp->flags &= ~PF_NO_REQUESTS;
        break;
    case NB_INTEREST:
        btpd_log(BTPD_L_MSG, "sent interest to %p\n", p);
        break;
    case NB_UNINTEREST:
        btpd_log(BTPD_L_MSG, "sent uninterest to %p\n", p);
        break;
    case NB_HAVE:
        btpd_log(BTPD_L_MSG, "sent have(%u) to %p\n",
            nb_get_index(nb), p);
        break;
    case NB_BITFIELD:
        btpd_log(BTPD_L_MSG, "sent bitfield to %p\n", p);
        break;
    case NB_REQUEST:
        btpd_log(BTPD_L_MSG, "sent request(%u,%u,%u) to %p\n",
            nb_get_index(nb), nb_get_begin(nb), nb_get_length(nb), p);
        break;
    case NB_PIECE:
        btpd_log(BTPD_L_MSG, "sent piece(%u,%u,%u) to %p\n",
            nb_get_index(nb), nb_get_begin(nb), nb_get_length(nb), p);
        break;
    case NB_CANCEL:
        btpd_log(BTPD_L_MSG, "sent cancel(%u,%u,%u) to %p\n",
            nb_get_index(nb), nb_get_begin(nb), nb_get_length(nb), p);
        break;
    case NB_TORRENTDATA:
        btpd_log(BTPD_L_MSG, "sent data to %p\n", p);
        assert(p->npiece_msgs > 0);
        p->npiece_msgs--;
        break;
    case NB_MULTIHAVE:
        btpd_log(BTPD_L_MSG, "sent multihave to %p\n", p);
        break;
    case NB_BITDATA:
        btpd_log(BTPD_L_MSG, "sent bitdata to %p\n", p);
        break;
    case NB_SHAKE:
        btpd_log(BTPD_L_MSG, "sent shake to %p\n", p);
        break;
    }
}

void
peer_request(struct peer *p, struct block_request *req)
{
    assert(p->nreqs_out < MAXPIPEDREQUESTS);
    p->nreqs_out++;
    BTPDQ_INSERT_TAIL(&p->my_reqs, req, p_entry);
    peer_send(p, req->msg);
}

int
peer_requested(struct peer *p, uint32_t piece, uint32_t block)
{
    uint32_t begin = block * PIECE_BLOCKLEN;
    struct block_request *req;
    BTPDQ_FOREACH(req, &p->my_reqs, p_entry)
        if (nb_get_index(req->msg) == piece && nb_get_begin(req->msg) == begin)
            return 1;
    return 0;
}

void
peer_keepalive(struct peer *p)
{
    peer_send(p, nb_create_keepalive());
}

void
peer_cancel(struct peer *p, struct block_request *req, struct net_buf *nb)
{
    BTPDQ_REMOVE(&p->my_reqs, req, p_entry);
    p->nreqs_out--;

    int removed = 0;
    struct nb_link *nl;
    BTPDQ_FOREACH(nl, &p->outq, entry) {
        if (nl->nb == req->msg) {
            removed = peer_unsend(p, nl);
            break;
        }
    }
    if (!removed)
        peer_send(p, nb);
    if (p->nreqs_out == 0)
        peer_on_no_reqs(p);
}

void
peer_unchoke(struct peer *p)
{
    p->mp->flags &= ~PF_I_CHOKE;
    peer_send(p, nb_create_unchoke());
}

void
peer_choke(struct peer *p)
{
    struct nb_link *nl = BTPDQ_FIRST(&p->outq);
    while (nl != NULL) {
        struct nb_link *next = BTPDQ_NEXT(nl, entry);
        if (nl->nb->type == NB_PIECE) {
            struct nb_link *data = next;
            next = BTPDQ_NEXT(next, entry);
            if (peer_unsend(p, nl))
                peer_unsend(p, data);
        }
        nl = next;
    }

    p->mp->flags |= PF_I_CHOKE;
    peer_send(p, nb_create_choke());
}

void
peer_want(struct peer *p, uint32_t index)
{
    if (!has_bit(p->piece_field, index) || peer_has_bad(p, index))
        return;
    assert(p->nwant < p->npieces);
    p->nwant++;
    if (p->nwant == 1) {
        p->mp->flags |= PF_I_WANT;
        if (p->mp->flags & PF_SUSPECT)
            return;
        if (p->nreqs_out == 0) {
            assert((p->mp->flags & PF_DO_UNWANT) == 0);
            int unsent = 0;
            struct nb_link *nl = BTPDQ_LAST(&p->outq, nb_tq);
            if (nl != NULL && nl->nb->type == NB_UNINTEREST)
                unsent = peer_unsend(p, nl);
            if (!unsent)
                peer_send(p, nb_create_interest());
        } else {
            assert((p->mp->flags & PF_DO_UNWANT) != 0);
            p->mp->flags &= ~PF_DO_UNWANT;
        }
    }
}

void
peer_unwant(struct peer *p, uint32_t index)
{
    if (!has_bit(p->piece_field, index) || peer_has_bad(p, index))
        return;
    assert(p->nwant > 0);
    p->nwant--;
    if (p->nwant == 0) {
        p->mp->flags &= ~PF_I_WANT;
        if (p->mp->flags & PF_SUSPECT)
            return;
        p->t_nointerest = btpd_seconds;
        if (p->nreqs_out == 0)
            peer_send(p, nb_create_uninterest());
        else
            p->mp->flags |= PF_DO_UNWANT;
    }
}

static struct peer *
peer_create_common(int sd)
{
    struct peer *p = btpd_calloc(1, sizeof(*p));

    p->mp = mp_create();
    mp_hold(p->mp);
    p->mp->p = p;

    p->sd = sd;
    p->mp->flags = PF_I_CHOKE | PF_P_CHOKE;
    p->t_created = btpd_seconds;
    p->t_lastwrite = btpd_seconds;
    p->t_nointerest = btpd_seconds;
    BTPDQ_INIT(&p->my_reqs);
    BTPDQ_INIT(&p->outq);

    peer_set_in_state(p, SHAKE_PSTR, 28);

    btpd_ev_new(&p->ioev, p->sd, EV_READ, net_io_cb, p);

    BTPDQ_INSERT_TAIL(&net_unattached, p, p_entry);
    net_npeers++;
    return p;
}

void
peer_create_in(int sd)
{
    struct peer *p = peer_create_common(sd);
    p->mp->flags |= PF_INCOMING;
}

void
peer_create_out(struct net *n, const uint8_t *id,
    const char *ip, int port)
{
    int sd;
    struct peer *p;

    if (net_connect_name(ip, port, &sd) != 0)
        return;

    p = peer_create_common(sd);
    p->n = n;
    peer_send(p, nb_create_shake(n->tp));
}

void
peer_create_out_compact(struct net *n, int family, const char *compact)
{
    int sd;
    struct peer *p;
    struct sockaddr_storage addr;
    struct sockaddr_in *a4;
    struct sockaddr_in6 *a6;
    socklen_t addrlen;

    switch (family) {
    case AF_INET:
        if (!net_ipv4)
            return;
        a4 = (struct sockaddr_in *)&addr;
        a4->sin_family = AF_INET;
        addrlen = sizeof(*a4);
        bcopy(compact, &a4->sin_addr.s_addr, 4);
        bcopy(compact + 4, &a4->sin_port, 2);
        break;
    case AF_INET6:
        if (!net_ipv6)
            return;
        a6 = (struct sockaddr_in6 *)&addr;
        a6->sin6_family = AF_INET6;
        addrlen = sizeof(*a6);
        bcopy(compact, &a6->sin6_addr, 16);
        bcopy(compact + 16, &a6->sin6_port, 2);
        break;
    default:
        abort();
    }
    if (net_connect_addr(family, (struct sockaddr *)&addr, addrlen, &sd) != 0)
        return;

    p = peer_create_common(sd);
    p->n = n;
    peer_send(p, nb_create_shake(n->tp));
}

void
peer_on_no_reqs(struct peer *p)
{
    if ((p->mp->flags & PF_DO_UNWANT) != 0) {
        assert(p->nwant == 0);
        p->mp->flags &= ~PF_DO_UNWANT;
        peer_send(p, nb_create_uninterest());
    }
}

void
peer_on_keepalive(struct peer *p)
{
    btpd_log(BTPD_L_MSG, "received keep alive from %p\n", p);
}

void
peer_on_shake(struct peer *p)
{
    uint8_t printid[21];
    int i;
    for (i = 0; i < 20 && isprint(p->mp->id[i]); i++)
        printid[i] = p->mp->id[i];
    printid[i] = '\0';
    btpd_log(BTPD_L_MSG, "received shake(%s) from %p\n", printid, p);
    p->piece_field = btpd_calloc(1, (int)ceil(p->n->tp->npieces / 8.0));
    if (cm_pieces(p->n->tp) > 0) {
        if ((cm_pieces(p->n->tp) * 9 < 5 +
                ceil(p->n->tp->npieces / 8.0)))
            peer_send(p, nb_create_multihave(p->n->tp));
        else {
            peer_send(p, nb_create_bitfield(p->n->tp));
            peer_send(p, nb_create_bitdata(p->n->tp));
        }
    }

    mptbl_insert(p->n->mptbl, p->mp);
    BTPDQ_REMOVE(&net_unattached, p, p_entry);
    BTPDQ_INSERT_HEAD(&p->n->peers, p, p_entry);
    p->mp->flags |= PF_ATTACHED;
    p->n->npeers++;

    ul_on_new_peer(p);
    dl_on_new_peer(p);
}

void
peer_on_choke(struct peer *p)
{
    btpd_log(BTPD_L_MSG, "received choke from %p\n", p);
    if ((p->mp->flags & PF_P_CHOKE) != 0)
        return;
    else {
        p->mp->flags |= PF_P_CHOKE;
        dl_on_choke(p);
        struct nb_link *nl = BTPDQ_FIRST(&p->outq);
        while (nl != NULL) {
            struct nb_link *next = BTPDQ_NEXT(nl, entry);
            if (nl->nb->type == NB_REQUEST)
                peer_unsend(p, nl);
            nl = next;
        }
    }
}

void
peer_on_unchoke(struct peer *p)
{
    btpd_log(BTPD_L_MSG, "received unchoke from %p\n", p);
    if ((p->mp->flags & PF_P_CHOKE) == 0)
        return;
    else {
        p->mp->flags &= ~PF_P_CHOKE;
        dl_on_unchoke(p);
    }
}

void
peer_on_interest(struct peer *p)
{
    btpd_log(BTPD_L_MSG, "received interest from %p\n", p);
    if ((p->mp->flags & PF_P_WANT) != 0)
        return;
    else {
        p->mp->flags |= PF_P_WANT;
        ul_on_interest(p);
    }
}

void
peer_on_uninterest(struct peer *p)
{
    btpd_log(BTPD_L_MSG, "received uninterest from %p\n", p);
    if ((p->mp->flags & PF_P_WANT) == 0)
        return;
    else {
        p->mp->flags &= ~PF_P_WANT;
        p->t_nointerest = btpd_seconds;
        ul_on_uninterest(p);
    }
}

void
peer_on_have(struct peer *p, uint32_t index)
{
    btpd_log(BTPD_L_MSG, "received have(%u) from %p\n", index, p);
    if (!has_bit(p->piece_field, index)) {
        set_bit(p->piece_field, index);
        p->npieces++;
        dl_on_piece_ann(p, index);
    }
}

void
peer_on_bitfield(struct peer *p, const uint8_t *field)
{
    btpd_log(BTPD_L_MSG, "received bitfield from %p\n", p);
    assert(p->npieces == 0);
    bcopy(field, p->piece_field, (size_t)ceil(p->n->tp->npieces / 8.0));
    for (uint32_t i = 0; i < p->n->tp->npieces; i++) {
        if (has_bit(p->piece_field, i)) {
            p->npieces++;
            dl_on_piece_ann(p, i);
        }
    }
}

void
peer_on_piece(struct peer *p, uint32_t index, uint32_t begin,
    uint32_t length, const char *data)
{
    struct block_request *req;
    BTPDQ_FOREACH(req, &p->my_reqs, p_entry)
        if ((nb_get_begin(req->msg) == begin &&
                nb_get_index(req->msg) == index &&
                nb_get_length(req->msg) == length))
            break;
    if (req != NULL) {
        btpd_log(BTPD_L_MSG, "received piece(%u,%u,%u) from %p\n",
            index, begin, length, p);
        assert(p->nreqs_out > 0);
        p->nreqs_out--;
        BTPDQ_REMOVE(&p->my_reqs, req, p_entry);
        if (p->nreqs_out == 0)
            peer_on_no_reqs(p);
        dl_on_block(p, req, index, begin, length, data);
    } else
        btpd_log(BTPD_L_MSG, "discarded piece(%u,%u,%u) from %p\n",
            index, begin, length, p);
}

void
peer_on_request(struct peer *p, uint32_t index, uint32_t begin,
    uint32_t length)
{
    btpd_log(BTPD_L_MSG, "received request(%u,%u,%u) from %p\n",
        index, begin, length, p);
    if ((p->mp->flags & PF_NO_REQUESTS) == 0) {
        peer_send(p, nb_create_piece(index, begin, length));
        peer_send(p, nb_create_torrentdata());
        p->npiece_msgs++;
        if (p->npiece_msgs >= MAXPIECEMSGS) {
            peer_send(p, nb_create_choke());
            peer_send(p, nb_create_unchoke());
            p->mp->flags |= PF_NO_REQUESTS;
        }
    }
}

void
peer_on_cancel(struct peer *p, uint32_t index, uint32_t begin,
    uint32_t length)
{
    btpd_log(BTPD_L_MSG, "received cancel(%u,%u,%u) from %p\n",
        index, begin, length, p);
    struct nb_link *nl;
    BTPDQ_FOREACH(nl, &p->outq, entry)
        if (nl->nb->type == NB_PIECE
            && nb_get_begin(nl->nb) == begin
            && nb_get_index(nl->nb) == index
            && nb_get_length(nl->nb) == length) {
            struct nb_link *data = BTPDQ_NEXT(nl, entry);
            if (peer_unsend(p, nl))
                peer_unsend(p, data);
            break;
        }
}

void
peer_on_tick(struct peer *p)
{
    if (p->mp->flags & PF_BANNED)
        goto kill;
    if (p->mp->flags & PF_ATTACHED) {
        if (BTPDQ_EMPTY(&p->outq)) {
            if (btpd_seconds - p->t_lastwrite >= 120)
                peer_keepalive(p);
        } else if (btpd_seconds - p->t_wantwrite >= 60) {
            btpd_log(BTPD_L_CONN, "write attempt timed out.\n");
            goto kill;
        }
        if ((cm_full(p->n->tp) && !(p->mp->flags & PF_P_WANT) &&
                btpd_seconds - p->t_nointerest >= 600)) {
            btpd_log(BTPD_L_CONN, "no interest for 10 minutes.\n");
            goto kill;
        }
    } else if (btpd_seconds - p->t_created >= 60) {
            btpd_log(BTPD_L_CONN, "hand shake timed out.\n");
            goto kill;
    }
    return;
kill:
    peer_kill(p);
}

void
peer_bad_piece(struct peer *p, uint32_t index)
{
    if (p->npcs_bad == 0) {
        assert(p->bad_field == NULL);
        p->bad_field = btpd_calloc(ceil(p->n->tp->npieces / 8.0), 1);
    }
    assert(!has_bit(p->bad_field, index));
    set_bit(p->bad_field, index);
    p->npcs_bad++;
    p->suspicion++;
    if (p->suspicion == 3) {
        btpd_log(BTPD_L_BAD, "suspect peer %p.\n", p);
        p->mp->flags |= PF_SUSPECT;
        if (p->nwant > 0) {
            p->mp->flags &= ~PF_DO_UNWANT;
            peer_send(p, nb_create_uninterest());
        }
    }
}

void
peer_good_piece(struct peer *p, uint32_t index)
{
    if (peer_has_bad(p, index)) {
        assert(p->npcs_bad > 0);
        p->npcs_bad--;
        if (p->npcs_bad == 0) {
            free(p->bad_field);
            p->bad_field = NULL;
        } else
            clear_bit(p->bad_field, index);
    }
    p->suspicion = 0;
    if (p->mp->flags & PF_SUSPECT) {
        btpd_log(BTPD_L_BAD, "unsuspect peer %p.\n", p);
        p->mp->flags &= ~PF_SUSPECT;
        if (p->nwant > 0) {
            assert(p->mp->flags & PF_I_WANT);
            peer_send(p, nb_create_interest());
        }
        if (peer_leech_ok(p))
            dl_on_download(p);
    }
}

int
peer_chokes(struct peer *p)
{
    return p->mp->flags & PF_P_CHOKE;
}

int
peer_has(struct peer *p, uint32_t index)
{
    return has_bit(p->piece_field, index);
}

int
peer_has_bad(struct peer *p, uint32_t index)
{
    return p->bad_field != NULL && has_bit(p->bad_field, index);
}

int
peer_laden(struct peer *p)
{
    return p->nreqs_out >= MAXPIPEDREQUESTS;
}

int
peer_wanted(struct peer *p)
{
    return (p->mp->flags & PF_I_WANT) == PF_I_WANT;
}

int
peer_leech_ok(struct peer *p)
{
    return (p->mp->flags & (PF_BANNED|PF_SUSPECT|PF_I_WANT|PF_P_CHOKE))
        == PF_I_WANT && !peer_laden(p);
}

int
peer_active_down(struct peer *p)
{
    return peer_leech_ok(p) || p->nreqs_out > 0;
}

int
peer_active_up(struct peer *p)
{
    return (p->mp->flags & (PF_P_WANT|PF_I_CHOKE)) == PF_P_WANT
        || p->npiece_msgs > 0;
}

int
peer_full(struct peer *p)
{
    return p->npieces == p->n->tp->npieces;
}

int
peer_requestable(struct peer *p, uint32_t index)
{
    return peer_has(p, index) && !peer_has_bad(p, index);
}
