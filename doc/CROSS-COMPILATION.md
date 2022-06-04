```
                  small,
        `___`         embeddable
        (O,O)             and
        \)  )          purely
      ---"-"---     functional!
   O t u s L i s p
```
*Based on Aki Helin's [Owl-Lisp](https://gitlab.com/owl-lisp/owl)*

CROSS-COMPILATION
=================

I'm personally compile and test Ol for: 
- i686,
- x86_64,
- ppc,
- ppc64,
- mips,
- arm,
- aarch64.

At the same time, I use only one operating system - Linux Mint on the x86_64 platform. So, how to compile and test all this binaries?

## Requirements

- `git`
- `curl`
- One of chroot: `uunshare`, `uchroot`, `bwrap` or `ethereal` (can be changed using XBPS_CHROOT_CMD variable in etc/defaults.conf).
- common POSIX utilities included by default in almost all UNIX systems

Full list of requirements can be found at the [void-packages requirements](https://github.com/void-linux/void-packages#requirements) link.

- `qemu` for run/debug.


## Preparation

1. Setup [void-packages](https://github.com/void-linux/void-packages) repository.
```bash
$ git clone https://github.com/void-linux/void-packages --depth 1
$ cd void-packages
```

2. Setup [xbps](https://github.com/void-linux/xbps.git) tool.
```bash
# I made it as a submodule to have "all in one folder".
$ git submodule add https://github.com/void-linux/xbps.git
$ cd xbps
$ ./configure --enable-rpath --prefix=/usr --sysconfdir=/etc
$ make
# I use "local" subfolder for compiled xbps binaries.
$ make DESTDIR=`pwd`/local install clean
$ export PATH=`pwd`/local/usr/bin:$PATH
```

3. Configure.
```bash
# this step will take a time and lot of SSD space
$ ./xbps-src binary-bootstrap
```

4. Build Ol for host platform.
```bash
$ ./xbps-src pkg ol
```

## Cross-compilation

```bash
$ ./xbps-src -a $arch clean ol
$ ./xbps-src -a $arch -C pkg ol
```

The `arch` variable should be a:
- aarch64
- aarch64-musl
- armv5te
- armv5te-musl
- armv5tel
- armv5tel-musl
- armv6hf
- armv6hf-musl
- armv6l
- armv6l-musl
- armv7hf
- armv7hf-musl
- armv7l
- armv7l-musl
- i686
- i686-musl
- mips-musl
- mipsel-musl
- mipselhf-musl
- mipshf-musl
- ppc
- ppc-musl
- ppc64
- ppc64-musl
- ppc64le
- ppc64le-musl
- ppcle
- ppcle-musl
- x86_64
- x86_64-musl

A lot of, yeah?

Compiled binary `ol` will be located in the `./masterdir/usr/$(folder $arch)/usr/bin` folder.

I created a simple shell function to obtain the binary path:
```bash
folder() {
    case $1 in
		aarch64-musl)
			echo aarch64-linux-musl;;
		aarch64)
			echo aarch64-linux-gnu;;

		armv5tel-musl)
			echo arm-linux-musleabi;;
		armv5tel)
			echo arm-linux-gnueabi;;
		armv5te-musl)
			echo arm-linux-musleabi;;
		armv5te)
			echo arm-linux-gnueabi;;
		armv6hf-musl)
			echo arm-linux-musleabihf;;
		armv6hf)
			echo arm-linux-gnueabihf;;
		armv6l-musl)
			echo arm-linux-musleabihf;;
		armv6l)
			echo arm-linux-gnueabihf;;
		armv7hf-musl)
			echo armv7l-linux-musleabihf;;
		armv7hf)aarch64-linux-musl
			echo armv7l-linux-gnueabihf;;
		armv7l-musl)
			echo armv7l-linux-musleabihf;;
		armv7l)
			echo armv7l-linux-gnueabihf;;

		i686-musl)
			echo i686-linux-musl;;
		i686)
			echo i686-pc-linux-gnu;;

		mipselhf-musl)
			echo mipsel-linux-muslhf;;
		mipsel-musl)
			echo mipsel-linux-musl;;
		mipshf-musl)
			echo mips-linux-muslhf;;
		mips-musl)
			echo mips-linux-musl;;

		ppc64le-musl)
			echo powerpc64le-linux-musl;;
		ppc64le)
			echo powerpc64le-linux-gnu;;
		ppc64-musl)
			echo powerpc64-linux-musl;;
		ppc64)
			echo powerpc64-linux-gnu;;
		ppcle-musl)
			echo powerpcle-linux-musl;;
		ppcle)
			echo powerpcle-linux-gnu;;
		ppc-musl)
			echo powerpc-linux-musl;;
		ppc)
			echo powerpc-linux-gnu;;

		x86_64-musl)
			echo x86_64-linux-musl;;
	esac
}
```

## Running

We will run our cross-compiled ol binary without deploying the full guest OS. Just using qemu, fuse-overlayfs, and void-packages toolchain.

```bash
# let's create another one universal function
qemu() {
    case $1 in
		aarch64-musl)
			echo qemu-aarch64-static;;
		aarch64)
			echo qemu-aarch64-static;;

		armv5tel-musl)
			echo qemu-arm-static;;
		armv5tel)
			echo qemu-arm-static;;
		armv5te-musl)
			echo qemu-arm-static;;
		armv5te)
			echo qemu-arm-static;;
		armv6hf-musl)
			echo qemu-arm-static;;
		armv6hf)
			echo qemu-arm-static;;
		armv6l-musl)
			echo qemu-arm-static;;
		armv6l)
			echo qemu-arm-static;;
		armv7hf-musl)
			echo qemu-arm-static;;
		armv7hf)aarch64-linux-musl
			echo qemu-arm-static;;
		armv7l-musl)
			echo qemu-arm-static;;
		armv7l)
			echo qemu-arm-static;;

		i686-musl)
			echo qemu-i386-static;;
		i686)
			echo qemu-i386-static;;

		mipselhf-musl)
			echo qemu-mipsel-static;;
		mipsel-musl)
			echo qemu-mipsel-static;;
		mipshf-musl)
			echo qemu-mips-static;;
		mips-musl)
			echo qemu-mips-static;;

		ppc64le-musl)
			echo qemu-ppc64le-static;;
		ppc64le)
			echo qemu-ppc64le-static;;
		ppc64-musl)
			echo qemu-ppc64-static;;
		ppc64)
			echo qemu-ppc64-static;;
		ppcle-musl)
			echo qemu-ppc-static;;
		ppcle)
			echo qemu-ppc64le-static;;
		ppc-musl)
			echo qemu-ppc-static;;
		ppc)
			echo qemu-ppc-static;;

		x86_64-musl)
			echo qemu-x86_64-static;;
	esac
}

# do the job
ROOT=`pwd`/testing/$arch

package=ol
version=2.3.5 # obtained from the "version" line of "./srcpkgs/$package/template" file

# qemu binary should be accessible by guest process
[ -s `pwd`/masterdir/usr/$(folder $arch)/$(qemu $arch) ] || cp `which $(qemu $arch)` `pwd`/masterdir/usr/$(folder $arch)

echo ---------------------------------------------------------------------------------
echo $arch

./xbps-src -a $arch clean $package
# do not delete intermediate files
./xbps-src -a $arch -C pkg $package || exit $?

echo -- package built-----------------------------------------------------------------
[ -d "$ROOT" ] || mkdir -p "$ROOT"

sleep 1
fuse-overlayfs \
    -o lowerdir=`pwd`/masterdir/destdir/$(folder $arch)/$package-$version \
    -o upperdir=`pwd`/masterdir/usr/$(folder $arch) \
    -o workdir=$ROOT \
    $ROOT

echo -- testing ----------------------------------------------------------------------
sleep 1
proot -R $ROOT /$(qemu $arch) /usr/bin/ol # we just run ol

# at this point you can see the Ol prompt and type "(syscall 63)" to see the guest os uname

echo -- done -------------------------------------------------------------------------
sleep 1
fusermount -u $ROOT
```

## Debug

Same as [running](#running), but with additonal steps:
- `$ xbps-src -a $arch -C pkg gdb`
- append *lowerdir* with ``:`pwd`/masterdir/destdir/$(folder $arch)/gdb-11.1 ``
- change proot to `$ proot -R $ROOT /$(qemu $arch) /usr/bin/gdb /usr/bin/ol`


## Further readings

Void-packages repository [README.md](https://github.com/void-linux/void-packages/blob/master/README.md),  
X Binary Package System repository [README.md](https://github.com/void-linux/xbps/blob/master/README.md),  
[fuse-overlayfs](https://github.com/containers/fuse-overlayfs),  
[proot](https://proot-me.github.io/) - a user-space implementation of chroot,  


That's all Folks!