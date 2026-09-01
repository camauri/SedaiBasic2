# La coppia da pubblicare

Due file e due immagini, da caricare **così come sono** in una cartella qualunque di un server:

    index.html        la pagina che racconta com'è fatto, con i numeri
    buddhabrot.html   la demo che gira nel browser, col modulo dentro
    buddhabrot.png    l'immagine di apertura
    convergence.gif   il filmato della convergenza

Non serve altro: nessun server applicativo, nessuna configurazione, nessuna dipendenza esterna.
I collegamenti fra le due pagine sono **relativi**, quindi la cartella funziona a qualunque
indirizzo la si metta, sottocartella compresa.

⛔ `buddhabrot.html` qui è una **copia** di quella accanto al sorgente, con in più il link di
ritorno a `index.html`. Il modulo WebAssembly che porta dentro viene riallineato da
`bash bas/demo/buddhabrot/verify_wasm.sh --bless`, e la stessa rete **rifiuta di passare** se la
copia porta un modulo diverso da quello che il sorgente compila oggi: una pagina che pubblica il
modulo della settimana scorsa sembra perfetta e mostra un'altra cosa.
