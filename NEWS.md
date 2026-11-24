# MONIKAraporty 6.0.0 (11.2026)

## Zmiany w infrastrukturze

* Całkowite przepisanie szablonów raportów szkolnych z modelu "6 szablonów po jednym na typ szkoły" na "jeden szablon 'parent' zaciągający poszczególne części raportu z plików 'child'"
* Dodanie nowego podrozdziału dla szkół, po których możliwe jest kontynuowanie nauki na studiach - "Kontynuowanie nauki na studiach w poszczególnych kierunkach"

## Zmiany wynikające z nowelizacji ustawy

Uwzględnienie w szablonach zmian wynikających ze zmian ustawowych (od edycji 2026), 

Są to zmiany dotyczące głównie KRUS oraz wynagrodzeń:

* Wskaźnik status edukacyjno-zawodowy bierze teraz pod uwagę zatrudnienie w rolnictwie
* W wormach pracy pojawiła się nowa kategoria "Tylko rolnik"
* Wynagrodzenia są teraz przeliczane do pełnego etatu i takie wartości są porównywane do średnich wynagrodzeń w powiecie
* Zmiany w tekście raportu biorące pod uwagę pojawienie się informacji o ubezpieczonych w KRUS oraz o nowym sposobie prezentacji wynagrodzeń

## Znaczące przyspieszenie generowania raportów

Osiągnięto kilkukrotne przyspieszenie generowania raportów (w porównaniu do edycji 2025) dzięki:

* Zmianie silnika LaTeX z `lulatex` na `pdflatex`
* Przeniesienie niektórych obliczeń i poleceń z szablonu do zewnętrznych plików (m.in. przeniesienie ładownania bibliotek poza szablon, przeniesienie wszystkich zmiennych przechowujących odmianę przez przypadki do zbioru danych - nie są już liczone w locie w szablonie)
* Zmianie argumentu `longtable` na `FALSE` dla tabel, które nie potrzebują przeliczania szerokości tabeli przed wyrenderowaniem (czyli większości tabel)
* Zmianie generatora grafik `cairo_pdf` na format rastrowy `png` dzięki czemu pomijane jest tworzenie grafik wektorowych, które muszą być za każdym razem parsowane i wbudowywane w dokument

## Inne zmiany

* Do szablonów dla Szkół policealnych i Liceów ogólnokształcących dla dorosłych dodano podrozdział "Kontynuowanie nauki na studiach w poszczególnych dyscyplinach w podziale na płeć"
* Zmiana kolorystyki wykresów na kolory z nowej palety. Dodatkowo, przepisano wszystkie funkcje rysujące wykresy tak, by używały funkcji z pakietu `LOSYkolory`
