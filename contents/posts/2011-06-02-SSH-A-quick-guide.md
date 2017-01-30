---
title: "SSH: A quick guide"
tags: Security
---

The secure shell or *ssh* is much more than a secure replacement for
telnet. Using ssh is not only secure but also convenient. We will have
a look at ssh in this article.  The objective is not to explain all
the features of ssh, for that you can consult the man page, but to
examine some of the key features and their use.  All the code in this
poset should work if you cut and paste (without the `$` prompt of
course) it on to the terminal. Also by ssh I mean [openssh]
throughout.

## Your .ssh directory

All the files used by ssh are inside the .ssh directory in your home
area. Here is a list of them and their use.

* known_hosts: This file contains the public keys of some of the
  hosts that you have logged in to.
* id_rsa:     [RSA] private key.
* id_rsa.pub: [RSA] public key.
* id_dsa:     [DSA] private key.
* id_dsa.pub: [DSA] public keys.
* authorized_keys: List of public keys of users who are authorised to
  access this account.

## Managing known_hosts

The known_hosts file contains the public keys of all the hosts that
you have logged in to. It is a good idea to get these known hosts from
a trusted source. When your ssh client contacts a server, it receives
public key of the server. If there is a mismatch, ssh warns you that
the key has changed. This could be due to a man-in-the-middle attack
or due to a system reinstallation. When you get such a message it is
better to be sure that there is no tampering. Be especially careful if
you in an unknown LAN or WiFi network like that of an airport or a
hotel. Having a trusted known_hosts file is a very good security
measure.


## Key based login.

Usually one uses ssh with passwords to login. Although this is secure
in the sense that the passwords sent are encrypted, it has all the
problems of password based authentication. An alternative is to use
public key/private key based authentication. The public key access is
more secure and in fact more convenient than the password based
access.  Here is the step by step procedure.

~~~ {.bash .numberLines}

$ ssh-keygen # Generate the public key/private key pair.

~~~

You will find the generated keys inside the .ssh directory. The files
with extension .pub are the public keys. Copy them into the
.ssh/authorized_keys file of the remote machine.

~~~ {.bash .numberLines}
$ scp .ssh/id_rsa.pub @remote:
$ ssh remote
ppk@remote: mkdir .ssh
ppk@remote: cat id_rsa.pub >> .ssh/authorized_keys
               # copy the key to the authorized_keys file.
ppk@remote: chmod 644 .ssh/authorized_keys
               # Ensure that it is readable only to user.

~~~

The last step is particularly important. Ssh will refuse to login if
it finds that the .ssh/authorized_keys is writeable to someone other
than the user. Otherwise an intruder could leave his public key and
will have unrestricted access. So do not forget to change
permissions. Many have been stumped by this and ssh does not give any
indication on where the problem is.

In case you connect to many hosts it is a good idea to install the
same public key in all the different hosts you log into. Thus you need
to remember only one passphrase for all these hosts.

## Generating keys from a Windows machine

Of course the best option is to install yourself an operating system,
one of the BSD's or GNU/Linuxes for example. However if you don't have
that option, you will also be forced to use other ssh clients like
putty. My experience with these clients are limited and that prevents
me from giving a detailed procedure. Usually they have a click-click
interface to generate keys. The keys generated are however not in the
format expected by by openssh. Don't you worry. The correct format is
only a command line away.

As before you have to copy the public key to the remote machine.  The
command

~~~{.bash .numberLines}

$ ssh-keygen -i -f pubkeyfile

~~~

will convert an SSH2 compatible key format, which is what many of the
commercial ssh-client uses, to openssh compatible key format and print
it on the standard output. So after copying the public key to the
remote machine, you can type

~~~{.bash .numberLines}

$ ssh-keygen -i -f pubkeyfile >> .ssh/authorized_keys

~~~

on the remote machine.

## Passphrase, Empty Passphrase and SSH-agents.

While generating a public key/private key pair one is asked for a
passphrase. The passphrase is used to keep you private key encrypted
on the disk. It is never sent across the network or used in the
protocol.  Thus one can use an empty passphrase in which case the
private key is kept unencrypted on the disk. In case your machine is a
private laptop this is not such a bad idea.  The advantage of an empty
passphrase is that you will never have to type any passwords while
ssh-ing or scp-ing. However there is always a risk of your private key
getting compromised if the local machine from which you log on to the
remote machine is a shared machine. You could, for example, forget to
logout from the common terminal. So it is a good idea to have a
passphrase.

A better alternative to an empty passphrase is to use an
ssh-agent. The ssh-agent keeps you private key with it and does all
authentication on your behalf.  Here is a quick example.

~~~ {.bash .numberLines}

$ ssh-agent bash  # start a new shell session with an ssh-agent running
$ ssh-add         # add your public keys to the agent.
$ ssh remote      # No passphrase will be asked
ppk@remote: exit
$ scp foo ppk@remote:  # No passphrase will be asked.

~~~

I like to use ssh-agent in conjunction with screen (another cute
program). This is what I do.

~~~{.bash .numberLines}

$ ssh-agent screen # start a screen session with an ssh-agent
$ ssh-add          # in any of the windows of the screen.

~~~

Now no passwords are asked in any of the windows of the screen
session.  Usually I leave my screen session running in the office
machine (which is physically secure as only I have the key to my
office) and when I connect from home, I attach my self to the already
running screen in my office machine.

~~~ {.bash .numberLines}

ppk@home: ssh office
ppk@office: screen -x  # connect to the already running screen

~~~

When I am done I detach the screen. Thus one can go on for months
without typing any passphrase for any of the ssh/scp/sftp sessions.


## SSH-tunneling or port forwarding.

One of the most powerful uses of ssh is its ability to *port
forward*. You can build an *ssh tunnel* and connect a local port to a
remote port. For all purpose this local port is the remote port. For
example suppose there is an smtp server (mail server) running on
remote which relays mails only from remote. You can set up a tunnel
that connects your local port with that of the remote smtp port
provided you have shell access to the remote host. Here is how you do
it.

~~~ {.bash .numberLines}

$ ssh -N -L 2500:remote:25 ppk@remote &

~~~

Now you have a smtp server "running" at port 2500 of your machine.
All the traffic to port 2500 is redirected via the ssh tunnel to the
port 25 of the remote machine. If you want to actually forward the
port 25 of you local machine, you need to be root on your local
machine as this is a privileged port. However you don't need root
access on remote.

Using tunnel devices and ssh port forwarding one can also setup vpn
like network. We wont go into the details in this article.

[rsa]:  <http://en.wikipedia.org/wiki/RSA> "RSA Wiki"
[dsa]: <http://en.wikipedia.org/wiki/Digital_Signature_Algorithm> "DSA Wiki"
[openssh]: <http://www.openssh.com> "OpenSSH homepage"
