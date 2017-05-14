The change journal on NTFS is a file found at \$Extend\$UsnJrnl. It is a system metafile that is not possible to find in a normal dir listing, or even posible to by regular means. The relevant data is found in the alternate data stream named $J. There's also another ADS called $Max which is rather irrelevant. $J may be sparse, which would mean parts of the data is just 00's. This may be a significant portion of the total data, and most tools will extract this data stream to its full size (which is annoying and a huge waste of disk space). This is where this tools comes in, as it only extract the actual data for the change journal. That way extraction obviously also goes faster. Why extract 20 GB when you might only need 200 MB? 

Explanation of parameters
/ImageFile:
The full path and filename of an image file to extract from. If this param is used, then /ImageVolume: must be set. Optional.
/ImageVolume:
The volume number to extract from. If volume is not NTFS nothing will be extracted. Only used with /ImageFile:.
/DevicePath:
The full devicepath to extract from. Optional.
/OutputPath:
The output path to extract file to. Optional. If omitted, then extract path defaults to program directory.
/OutputName:
The output filename. Optional. If omitted, then filename be $UsnJrnl_$J.bin.


Input can thus be /ImageFile: or /DevicePath:. Image files must be raw dd like images, and be disk or partition type image. When specifying device paths in /DevicePath it is possible to access attached devices that does not have any volumes mounted. Examples are HarddiskVolume1, Harddisk0Partition2, HarddiskVolumeShadowCopy1, PhysicalDrive1. For PhysicalDriveN you must specify /ImageVolume param. The default is to extract the UsnJrnl to $UsnJrnl_$J.bin in program directory. If the output filename already exist, the existing one will be renamed and have "renamed_[timestamp]" appended where [timestamp] is the current time when program is run.


Example usage:

ExtractUsnJrnl /ImageFile:e:\images\disk.dd /ImageVolume:1 /OutputPath:e:\temp /OutputName:UsnJrnl_vol1.bin
ExtractUsnJrnl /DevicePath:c:
ExtractUsnJrnl /DevicePath:\\.\HarddiskVolumeShadowCopy1 /OutputPath:e:\temp /OutputName:UsnJrnl_SC1.bin
ExtractUsnJrnl /DevicePath:\\.\Harddisk0Partition2 /OutputPath:e:\temp
ExtractUsnJrnl /DevicePath:\\.\PhysicalDrive0 /ImageVolume:3 /OutputPath:e:\temp