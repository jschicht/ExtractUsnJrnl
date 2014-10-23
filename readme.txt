The change journal on NTFS is a file found at \$Extend\$UsnJrnl. It is a system metafile that is not possible to find in a normal dir listing, or even posible to by regular means. The relevant data is found in the alternate data stream named $J. There's also another ADS called $Max which is rather irrelevant. $J may be sparse, which would mean parts of the data is just 00's. This may be a significant portion of the total data, and most tools will extract this data stream to its full size (which is annoying and a huge waste of disk space). This is where this tools comes in, as it only extract the actual data for the change journal. That way extraction obviously also goes faster. Why extract 20 GB when you might only need 200 MB? 

The tool is extremely easy to use. It will need the target volume/drive as parameter (for instance C:), and optionally also the wanted output directory. If no parameters are supplied, a box will pop up asking you to supply the volume/drive. The extracted journal will by default go into the same directory as the tool was launched from, unless an output directory is specified as a second parameter.


Example usage:

ExtractUsnJrnl.exe C:
ExtractUsnJrnl.exe C: D:\OutputDir\