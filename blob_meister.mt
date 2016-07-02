
# the idea here rather simple:
#  provide something akin to freenets chk system but without the encryption
#
# pass in an block that gets by the :Bytes guard and you get an sha256 hash back
# pass in an sha256 hash and the una will give you back a promise for the original bytes block
#
# usufull for keeping big binary blobs out of vats.
