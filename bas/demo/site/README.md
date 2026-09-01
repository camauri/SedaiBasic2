# The site, as it goes on a server

Upload this folder **as it is**, anywhere — a domain root, a subfolder, it does not matter:

    index.html                 the index of demos
    buddhabrot/                what it is, the demo, two images
    bubble_universe/           what it is, the demo, one image
    voxel_landscape/           what it is, the demo, one image

Every link is **relative**, every page can be reached from the index and every page can get back to
it, and each demo carries its WebAssembly module inside its own page. Nothing else is needed: no
application server, no configuration, no external dependency, no font or script fetched from
anywhere.

⛔ **`bash bas/demo/verify_site.sh` is what keeps that true**, and it is a row in `all_nets.sh`. It
checks four things, each of which has been shipped broken by somebody at some point:

1. every local `href` and `src` resolves to a file that exists — a relative link that goes nowhere
   is the easiest thing in the world to ship, because on the machine that wrote it the file was
   there a moment ago under another name;
2. every page is reachable from the index, and every page offers a way back to it;
3. each demo page instantiates its module **asynchronously** — a browser refuses synchronous
   compilation of anything over 4 KB on the main thread, which Node does not, so a page can run
   perfectly on the desktop that wrote it and fail on every phone — and rebuilds its view of the
   module's memory, because a typed array over that memory is detached the moment it grows, with no
   exception raised and the canvas frozen on its last frame;
4. each page carries the module its source compiles to **today**. A page publishing last week's
   module looks perfectly fine and is showing something else.

`--bless` re-embeds every module after a source change.
