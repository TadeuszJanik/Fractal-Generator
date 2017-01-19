This is neither an efficient way of displaying graphics nor an efficient program for doing that -
drawing a 1000x1000 fractal for a more complicated function might take an hour or more, so for efficiency
draw smaller pictures first to see if that's what you are looking for, then do it once more with a bigger picture
(and think of something productive while you are waiting for the picture to be drawn).

Warning - software supporting PPM format required (GIMP for example).

User Manual:
1. How to draw a picture?
Launch Main.hs, type "main", press Enter, unfinished picture should already appear in the same directory as Main.hs

2. How to change the picture's name before it's drawn?
Edit line 16 in Main.hs (writeFile)

3. How to design a fractal?
Open Image.hs in a text editor of your choice - lines 7-49 describe all the parameters you can modify.

4. What ComplexNumber operations are supported?
	~ addition       +
	~ subtraction    -
	~ multiplication *
	~ division       //      <- IMPORTANT // instead of /
	~ exp            complexExp
   Furthermore, there are functions like algebraicForm, expForm, derive, modulus or arg which might be of use in case you want to write your own functions.
   * to initialize some algebraic constants like 1+i, use constructor ComplexNumber 1 1

5. What can you draw with this software?
	~ generalized Newton's fractals -- switch shading in Image.hs to Iterative
	~ complex wheel plots -- switch shading to Exponential or None