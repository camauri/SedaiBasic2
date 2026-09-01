# The pair that gets published

Two pages and two images, uploaded **as they are** into any folder of any server:

    index.html            what it is, how it is built, and the numbers
    buddhabrot.html       the demo that runs in the browser, module inside
    bubble_universe.html  the second demo, same compiler, 9 KB of module
    buddhabrot.png        the opening picture
    convergence.gif       the convergence film

Nothing else is needed: no application server, no configuration, no external dependency. The links
between the two pages are **relative**, so the folder works at any address, subfolder included.

⛔ The two demo pages here are **copies** of the ones beside their sources, with the way back added.
The WebAssembly module each carries is realigned by `--bless` on its own net —
`bas/demo/buddhabrot/verify_wasm.sh` and `bas/demo/verify_bubble_wasm.sh` — and both nets **refuse to
pass** when a copy carries a different module than the source compiles to today: a page publishing
last week's module looks perfectly fine and is showing something else.
