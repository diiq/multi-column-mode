Multi-column-mode is inspired by two-column mode (C-x 6 2); it horizontally splits one buffer into two, so that you can edit separate columns in separate buffers, then recombine.

However, two-column mode doesn't work very well; it screws up the mode-line, uses strange markers, and only lets you edit two-column documents. 

Multi-column-mode doesn't care how many columns you use, and it leaves all your buffers squeaky clean.

To use, `M-x multi-column-mode` to enter the minor mode. Move to the column along which you'd like to split the document. Then hit `C-|`. 
You'll suddenly have two buffers, each with half your original. 

Edit your documents as you please. Note that the longest line in each buffer will determine the width of your column when the buffers are re-merged.

To re-merge, use `M-]` (merge this buffer with the one to its left (or previous window)) or `M-[` (merge this buffer with the one to its right (or next window)). The only buffers that will merge are ones in multi-column mode, and ANY buffers in multi-column-mode will merge if you ask them to! 

At the moment, it's very preliminary, so watch out for extra spaces hanging over the end of your column -- that'll screw stuff up -- and if you split a document with some columnar data and some full-width, that'll be pretty messy too. Soon to be fixed.

