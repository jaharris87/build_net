REACLIB Reaction Network Generator for XNet
============

Tools for creating customized reaction networks for XNet from JINA REACLIB rates and FFN weak rates.

## Getting started

The file `input` contains controls for incorporating non-REACLIB rates and the path for the new `netsu`, `netwinv`, `netweak`, and `netneutr` files to be used by XNet.

The directory `./raw_data` contains the rate databases:
* ReaclibV2.X in Reaclib2 format w/o ch. 9/10/11)
  * Data Files:
    * `reaclib_JINAv2X` - Parameterized Rates
    * `winvne_JINAv2X`  - Tabulated Partition Functions
  * References:
    * Cyburt et al., ApJS 189 (2010) 240
    * https://groups.nscl.msu.edu/jina/reaclib/db/library.php?action=viewsnapshots
* Tabulated EC/PC rates in FFN-style formatting
  * Files:
    * `lmpffnoda.data` - Tabulated Rates
  * References:
    * Fuller et al., ApJ 293 (1985) 1
    * Oda et al., ADNDT 56 (1994) 231
    * Langanke & Martinez-Pinedo, ADNDT 79 (2001) 1
* Tabulated neutrino capture rates (not public)
  * Files:
    * `neutrino.data` - Tabulated Rates
  * References:
    * private communication with Carla Froehlich (2015)


## To run:

1. Compile the code:
  * `make`
2. Create a list of nuclei in the file `sunet` in the run directory that is a subset of nuclei in the REACLIB database
  * see `sunet.example` for correct format
3. Configure `input` file
4. Run the code:
  * `./build_net`


## Example(s)

See `sunet.example` for a simple example of an alpha-network with neutrons and protons

Some other useful sunet files are also included