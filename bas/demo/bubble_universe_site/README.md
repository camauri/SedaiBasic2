# The pair that gets published

Two pages and one image, uploaded **as they are** into any folder of any server:

    index.html            what it is, how it works, and the numbers
    bubble_universe.html  the demo that runs in the browser, module inside
    bubble_universe.png   the opening picture

Nothing else is needed: no application server, no configuration, no external dependency. The links
between the two pages are **relative**, so the folder works at any address, subfolder included.

⛔ This folder is BUBBLE UNIVERSE's, and the Buddhabrot has its own
(`bas/demo/buddhabrot/site/`). They are two demos with two stories, and one page carrying both makes
both of them worse — which is what happened for one commit, and why this note is here.

⛔ `bubble_universe.html` is a **copy** of the one beside the source, with the way back added. The
module it carries is realigned by `bash bas/demo/verify_bubble_wasm.sh --bless`, and the same net
**refuses to pass** when either copy carries a different module than the source compiles to today: a
page publishing last week's module looks perfectly fine and is showing something else.
