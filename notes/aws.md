# Notes on AWS setup

## My AWS Nodes:

There are two nodes, one is at 54.69.132.173 and hosts
appraisalscribe.com, happstack.com, seereason.com, etc.  It is listed
under the aws account ddssff@gmail.com.

The other is at 52.10.137.136, and can be logged into as the user
ubuntu@aws2.seereason.com with a suitable .pem key.

# Setting up a server on Amazon Web Services

Start at https://aws.amazon.com/console/

 * Click “Launch Instance”
 * Choose Ubuntu 16.04
 * Choose t2.micro -> (t2.small for 2GB ram)
 * Configure Instance Details -> Add Storage (enter 100GB)
 * Review & Launch
 * Choose a key
	I was unable to connect with SeeReasonAWSKey.pem (f993).  I also
	have AWSKey.pem (b3bd).  I had "AppraisalScribe Key", but I think
	I deleted it.  I will now create and use SeeReason.pem (e300).
 * New instance: i-0ce7f05cc8b706b86
 * ssh -i ~/.ssh/SeeReason.pem ubuntu@54.202.115.36

# Setting up a new instance (current)

 * sudo apt-get update; sudo apt-get install emacs git
 * Add swap:
     > dd if=/dev/zero count=4K bs=1M of=/boot/swap
     > mkswap /boot/swap
     > chmod 600 /boot/swap
     > swapon /boot/swap
     > echo "/boot/swap none swap sw 0 0" >> /etc/fstab
 * Install public seereason sources in /etc/apt/sources.list.d, install seereason-keyring
 * Set up root access to upload@deb.seereason.com and upload@deb8.seereason.com:
     - Copy id_rsa.pub (a308) and id_rsa (3c7e) from genie.local to /root/.ssh
 * In security groups -> launch-wizard2, open tcp ports 80, 443, and 9000-9500 (in sg-6db22311 aka launch-wizard-6.)
     (Current server uses sg-d585aab0.  I opened everything)
 * Edit /etc/sudoers (with visudo):  (why?)
	-Defaults       env_reset
	+Defaults       env_reset,env_keep=SSH_AUTH_SOCK
 * Sync appraisalscribe database:
     > rsync -aHxS upload@appraisalscribe.com:/srv/appraisalscribe-production/ /srv/appraisalscribe-production
     > sudo rm /srv/appraisalscribe-production/*/*/events-*.log
     > apt-get install appraisalscribe-production
 * Install and run appraisalscribe-production
    > sudo apt-get install appraisalscribe-production

      ubuntu@ip-172-31-38-200 - trusty - elastic: 54.69.132.173
      ubuntu@ip-172-31-11-197 - xenial - elastic: 34.213.34.241 (temporary 54.190.13.79)
      

# Moving appraisalscribe production to new server
 * Get new server running and almost up to date
 * shut down old server
     - /etc/init.d/appraisalscribe-production stop
 * sync to new server
     - rsync -aHxS upload@appraisalscribe.com:/srv/appraisalscribe-production/ /srv/appraisalscribe-production
 * start new server
     - /etc/init.d/appraisalscribe-production start
 * flip elastic ip to new server
      - Disassociate Elastic IP 52.10.137.136 from i-0058534c28089dfa9 (ip-172-31-38-200)
      - Associate elastic ip with i-0ce7f05cc8b706b86

# Allow ssh upload@deb.seereason.com from aws node without a password

 * Need to get ssh-agent to run and not die when I log out

# Move our deb repo to aws node

 * Create user upload (adduser upload, old linspire password)
 * Copy .ssh and .gnupg directories from current deb.seereason.com to ~upload
 * Install newdist, test with ‘newdist --root=/srv/deb/ubuntu’
 * Sync /srv/deb and /srv/deb-private from deb.seereason.com
 * Setup apache2 to serve files via deb.seereason.com:
	Install apache2 - this won’t start because test.appraisalscribe.com is running on port 80
 * Point deb.seereason.com to 54.69.132.173 in /etc/hosts and test
 * Finally, change dns for deb.seereason.com at dreamhost

# Safely reboot after dist upgrade and kernel install

Dist upgrade gives this diff for menu.lst, I chose to install the maintainer version:

Line by line differences between versions

10a
>>>>>>> New
.
9a
<<<<<<< Older
kernel /boot/vmlinuz-3.13.0-44-generic root=/dev/hda1 ro single
=======
.
8c
title Ubuntu 14.04.2 LTS, kernel 3.13.0-44-generic (recovery mode)
.
5a
||||||| Older
kernel /boot/vmlinuz-3.13.0-44-generic root=/dev/hda1 ro console=hvc0
=======
kernel /boot/vmlinuz-3.13.0-45-generic root=LABEL=cloudimg-rootfs ro console=hvc0
initrd /boot/initrd.img-3.13.0-45-generic

title Ubuntu 14.04.2 LTS, kernel 3.13.0-45-generic (recovery mode)
root (hd0)

kernel /boot/vmlinuz-3.13.0-45-generic root=LABEL=cloudimg-rootfs ro single
initrd /boot/initrd.img-3.13.0-45-generic

title Ubuntu 14.04.2 LTS, kernel 3.13.0-44-generic
root (hd0)
kernel /boot/vmlinuz-3.13.0-44-generic root=LABEL=cloudimg-rootfs ro console=hvc0
>>>>>>> New
.
4a
<<<<<<< Current
.
3c
title Ubuntu 14.04.2 LTS, kernel 3.13.0-45-generic
.


	On EC2 console, reboot running instance (success this time!  Do not
	sudo shutdown -r!)  From
	https://us-west-2.console.aws.amazon.com/ec2/v2/home?region=us-west-2#Instances:,
	 running instances, actions -> instance state -> reboot

## Expand root volume from 8gb to 30gb

 * http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-expand-volume.html
 * Set instance shutdown behavior to Stop
 * Stop instance
 * Snapshot the volume, wait till completed
 * Select and click “create volume”, set size (30gb) and
	availability zone to match instance (us-west-2b?)
 * Detach old volume, attach new volume as /dev/sda1
 * Set instance state to Start

## Using the .pem file

Getting SSH communication to work (for apt-get update etc)
Allocating an Elastic IP address
Setting up an S3 bucket to receive usage reports:
      https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/usage-reports.html

Pricing:

 * 30GB storage free (for 1 year?)
 * 1 GB ram free (for 1 year?)
 * 1 Elastic IP free (for 1 year?)
