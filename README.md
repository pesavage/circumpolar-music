# circumpolar-music
Data and code underlying Savage et al. (2015). How “circumpolar” is Ainu music? Musical and genetic perspectives on the history of the Japanese archipelago.

This code was used on 2013-11-12 to perform the analyses of 680 traditional songs from 35 populations published in:

Savage, P. E., Matsumae, H., Oota, H., Stoneking, M., Currie, T. E., Tajima, A., Gillan, M., & Brown, S. (2015). How “circumpolar” is Ainu music? Musical and genetic perspectives on the history of the Japanese archipelago. Ethnomusicology Forum, 24(3), 443–467. https://doi.org/10.1080/17411912.2015.1084236

The code to create musical distance matrices is based on code developed for:

Rzeszutek, T., Savage, P. E., & Brown, S. (2012). The structure of cross-cultural musical diversity. Proceedings of the Royal Society B: Biological Sciences, 279(1733), 1606–1612. https://doi.org/10.1098/rspb.2011.1750

Please cite these publications when adapting this code.

To run the analyses:
1) download the "Savage_Et_Al_Ainu_Music_Supplementary.csv" file
2) run the "AinuMusic.R" script to calculate musical distances among all 680 songs
3) run the "AinuMusicPhiST.R" script (including associated use of the Arlequin program) to calculate musical PhiST distances among the 35 populations
4) for convenience, the resulting outputs of both of these calulations are provided as "Japan680SongDist.csv" and "Japan680Music.csv", respectively.
