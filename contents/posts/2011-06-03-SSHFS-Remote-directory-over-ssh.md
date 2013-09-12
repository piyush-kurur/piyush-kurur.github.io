---
title: SSHFS: Remote directory over ssh
tags: Security
---

Often one wants shared access to files across machines. Traditionally
one uses the [network file system][nfs] ([nfs]). The network file
server works as follows: There is an *nfs server* that *exports* some
directories in its filesystem hiearchy to various *nfs clients* that
*mount* these directory over the network into their file system
hierarchy. As a result, each of the clients shares the directories
exported by the nfs server. However [nfs] is probably the worst
protocol when it comes to security and is rightly called network
failure system.

This is a tutorial on sshfs or ssh file system. The idea is to provide
a [nfs] like mount which is secured by the very dependable ssh (the
sftp subsystem of ssh).


# Using sshfs.

1. First mount the remote directory onto a local directory

~~~ {.bash .numberLines}

$ sshfs ppk@remote: path/to/mount

~~~

   where `path/to/mount` is the point where you want the remote file
   system to be mounted.


2. After step 1, `path/to/mount` on your local machine is actually the
   home directory of the remote machine. So you can use it just like a
   local machine. Expect slow response if your network connection to
   remote machine is slow though.

~~~ {.bash .numberLines}
$ cd path/to/mount
$ emacs myfavoritprogram.hs
$ ghc myfavoritprogram.hs
~~~


3. After you are done with the work on the remote machine you may
   unmount the file system

~~~ {.bash .numberLines}

$ fusermount -u path/to/mount

~~~


# How it works.

Sshfs is a userspace file system (fuse) that works over ssh, or rather
sftp. Fuse is an implementation of filesystem primitives in userspace
rather than in kernel space. This essentially means that users can
mount and unmount file system without having to be root. Sshfs makes
use of the sftp subsystem to do the remote file system
operations. Thus all the great features of ssh holds true, i.e. key
based authentication, use of ssh-agents. See my
[tutorial blog on ssh][ssh-tutorial] for more details on how to use
ssh.

# Installing sshfs.

All linux distros have a prebuilt package for sshfs.  On
[Debian]/[Ubuntu] and [Arch] the relevant package is `sshfs`. So all
you need to do is to install it.

~~~ {.bash .numberLines}

$ aptitude install sshfs # as root.
$ sudo aptitude install sshfs # if you are on Unbutu
$ pacman -S sshfs # as root on an Arch machine

~~~

On Fedora it looks like it is called `fuse-sshfs` so something like
this should work.

~~~ {.bash .numberLines}
$ yum install fuse-sshfs
~~~

# Ssh is working but not sshfs.

A common error that people have reported is that ssh works but sshfs
fails. If this happens, check whether your sftp subsystem is working.
Most probably this too would fail or work incorrectly. One of the main
reasons why sshfs/sftp does not work is because your startup scripts
in the remote machine prints stuff on the screen.  To check this out,
try the following command.

~~~ {.bash .numberLines}
$ ssh ppk@remote /bin/true
~~~

If this command produces any output then you are in trouble. You have
to fix your startup script in your remote machine --- `.bash_profile`
and `.bashrc`, if you are using bash as your default shell. The
startup script should check whether the standard output is a terminal
before it outputs something. For this protect your output generating
commands inside a `test -t 1` block as follows

~~~ {.bash .numberLines}
$ cat .bash_profile

if [ -t 1 ] # Check if stdout is connected to a terminal
then
    echo "The answer is 42"
fi

~~~
See the [openssh FAQ][sshfaq] for more details.


[sshfaq]: <http://www.openssh.com/faq.html>
	  "ssh faq"

[ssh-tutorial]: </posts/2011-06-02-SSH-A-quick-guide.html>

[ubuntu]: <http://www.ubuntu.com> "The Ubuntu homepage"
[debian]: <http://www.debian.org> "The Debian homepage"
[arch]: <http://www.archlinux.org> "The Arch Linux homepage"
[nfs]: <http://en.wikipedia.org/wiki/Network_File_System_(protocol)>
	"NFS Wiki"
