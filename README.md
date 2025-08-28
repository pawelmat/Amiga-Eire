# Amiga-Eire
Amiga 500 OCS 40k intro by Suspect and Scoopex (and Casyopea)  
Code: Kane / Suspect  
Graphics: Sim / Scoopex  
Music: Bartesek / Casyopea  

Ranked #2 in the 40k Amiga 500 intro category at Boom! party 2025

Requires off the shelf Amiga 500 OCS with 0.5 MB Chip and 0.5 MB other memory to run. Can run on 0.5 MB Chip only if nothing else is loaded.

# How to build and run
- Load workspace into Visual Studio Code
- Install the Amiga Assembly extension. Default setting should be fine although you might want to provide your own licensed Kickstart 1.3
- Press F5 to assemble and run in the embedded WinUAE. Uses vlink and vasmm68k_mot from VASM (embedded in the extension)

The output files are created in uae/dh0 which is automatically mapped as DH0 in the UAE embedded in the extension. 

Additionally you can press Ctrl-Shift-B to show the list of tasks configured in the project.
- 'build_debug' builds a debug version with symbols
- 'build_release' builds the release version with no symbols
- 'shrink runs' shrinkler (embedded in the project) to pack the executable (it also adds the .exe extension)
- 'prepare assets' runs a batch script preparing some of the assets - converting .iff and .mod files to what the intro requires. This has to be done only once.

# Acknowledgements

This project uses (all executables embedded in the project):
- Light Speed Player (LSPlayer): https://github.com/arnaud-carre/LSPlayer
- Shrinkler: https://github.com/askeksa/Shrinkler
- Iff to raw bitplane converter: https://github.com/pawelmat/Iff2Bpl
 
