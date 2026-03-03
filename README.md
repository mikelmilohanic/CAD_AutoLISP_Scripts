# UPUTE ZA POVEZIVANJE SKRIPTI S LOKALNOM INSTANCOM ZWCAD-a:
1. Otvoriti 'Loader_Files' direktorij.
2. Pokrenuti datoteku naziva 'LoaderDocument'.
3. Povući i ispustiti datoteku naziva 'MainLoaderLisp'
   unutar otvorenog prozora 'LoaderDocument'.
4. Prilikom dobivanja obavijesti o uspješnosti potrebno
   je zatvoriti sve otvorene instance ZWCAD-a te će sve
   skripte biti dostupne prilikom sljedećeg pokretanja
   ZWCAD-a.
5. Ukoliko navedeni postupak nije bio uspješan potrebno je
   pratiti dolje navedene korake unutar odjeljka
   "RUČNO PODEŠAVANJE".

# RUČNO PODEŠAVANJE:
*u slučaju neuspjelog prvog načina ili rada s drugim CAD
softverom koji nije ZWCAD.

1. Otvoriti instancu ZWCAD-a
2. Pokrenuti naredbu APPLOAD
3. Unutar otvorenog prozora kliknuti gumb 'Startup Suite'
4. Unutar prozora 'Startup Suite' kliknuti na gumb 'Add...'
5. Zatim se otvara prozor unutar kojeg je potrebno pronaći
   datoteku 'MainScript'. Navedena datoteka trebala bi se 
   nalaziti u direktoriju Scripts zajedno sa svim ostalim skriptama.
6. Prilikom pronalaska 'MainScript' datoteke istu je
   potrebno odabrati, potom kliknuti 'Otvori', a zatim 'OK'
   unutar ostalih otvorenih prozora.
7. Naposlijetku je potrebno zatvoriti sve otvorene instance
   ZWCAD-a te će sve skripte biti dostupne prilikom
   sljedećeg pokretanja ZWCAD-a.

# OSNOVNE NAREDBE ZA RAD S SKRIPTAMA
- INFO - ispis naredbi svih dostupnih skripti te
         informacija o tome što pojedina naredba radi
- RELOAD_LISPS - osvježavanje svih skripti