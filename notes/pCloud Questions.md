# Pcloud questions:

## Mental Model

### Upload via Drive?

1. Is it safe to simply move an enormous folder into the pCloud Drive?  Or is
   it better to move it into a pCloud Sync?

2. Suppose I create a sync whose local folder is empty and whose
   remote folder has many important files.  Will it copy the remote
   files to the local empty directory or will it make the remote
   directory empty?

3. What happens when I delete a Sync?  Does it leave the files on the disk?

What happens when you create a new sync to a directory with files in
it?  If the directory is already the same as what is in the cloud will
there still be downloads (yes, but why?)  Will files that do not match
be deleted?  (presumably.)   (Motivation - I created a sync on the
wrong drive, and wanted to move it.)

3. Does moving files and folders in a Drive folder always work?  What if
there are conflicting changes occurring in another version of that
Drive folder?  (I think I’ve seen an infinite loop in this case, I had
to kill the server and remove the .pcloud folder on that machine.)

If I create a new Sync of the same folder on the same machine will it
use LAN sync?  Is that better than deleting the old one before
creating the new one?

What happens if I (accidentally?) create a sync whose local folder is
a parent of other syncs?  (I did this, but I deleted it before bad
things happened.)

What happens if I create a sync whose local folder has some files in
it?  (Those files get added to the cloud folder.)  What if the local
file had the same name as a cloud file, but different contents?  (The
local file is renamed with the suffix “(conflicted)”)

What if you drag stuff into a sync directory and then kill the server?
 How about a drive directory?

What should I do if I drag some stuff into a Sync folder and then
decide its too big and I want to drag it back out again?  (Seems to
behave as one would wish - files restored, up and downloads stopped.)
In Settings -> Speed, what does “Auto” mean?

====================

Difference between console client and "atom" desktop client

  1. You don't get the nice UI with the console client
  2. No way to edit your syncs and drives(?)
