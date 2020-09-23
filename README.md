# imageCompressor
This project is a 2nd year one done at Epitech using Haskell language.  
The goal was to create an image compressor using the [k-means clustering algorithm](https://en.wikipedia.org/wiki/K-means_clustering).

## How to run it ?  
```
./imageCompressor n e IN
n number of colors in the final image
e convergence limit
IN path to the file containing the colors of the pixels
```
As you can see, you need to give a file as last argument which represents pixels and there postions in the image.  
Each line must be formatted this way:
```
IN ::= POINT ' ' COLOR ('\n' POINT ' ' COLOR ) *
POINT ::= '(' int ',' int ')'
COLOR ::= '(' SHORT ',' SHORT ',' SHORT ')'
SHORT ::= '0 '.. '255 '
```
Here is an example of a valid file:  
```
$> cat exampleIn
(0,0) (33,18,109)
(0,1) (33,18,109)
(0,2) (33,21,109)
(0,3) (33,21,112)
(0,4) (33,25,112)
(0,5) (33,32,112)
(1,0) (33,18,109)
(1,1) (35,18,109)
(1,2) (35,21,109)
(1,3) (38,21,112)
```
