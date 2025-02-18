#ifndef BTPD_CONTENT_H
#define BTPD_CONTENT_H

void cm_init(void);

void cm_create(struct torrent *tp, const char *mi);
void cm_kill(struct torrent *tp);

void cm_start(struct torrent *tp, int force_test);
void cm_stop(struct torrent * tp);

int cm_active(struct torrent *tp);
int cm_error(struct torrent *tp);
int cm_started(struct torrent *tp);
int cm_full(struct torrent *tp);

off_t cm_content(struct torrent *tp);
uint32_t cm_pieces(struct torrent *tp);

uint8_t *cm_get_piece_field(struct torrent *tp);
uint8_t *cm_get_block_field(struct torrent *tp, uint32_t piece);

int cm_has_piece(struct torrent *tp, uint32_t piece);

int cm_put_bytes(struct torrent *tp, uint32_t piece, uint32_t begin,
    const uint8_t *buf, size_t len);
int cm_get_bytes(struct torrent *tp, uint32_t piece, uint32_t begin,
    size_t len, uint8_t **buf);

void cm_prealloc(struct torrent *tp, uint32_t piece);
void cm_test_piece(struct torrent *tp, uint32_t piece);

#endif
