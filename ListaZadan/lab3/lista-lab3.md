## Przygotowanie danych

```
library(data.table)
library(caret)

load("KrukUWr2023.RData")

source("lab3-data-preparation.R")
```

## Zadanie #1

1.  Podziel zbiory na segmenty wykorzystując do tego zmienne ze zbiorów `cases~`:
    -   Podejście eksperckie. Zaproponuj własny podział spraw wg wybranego przez siebie kryterium. Wybierz samodzielnie liczbę segmentów. Segmenty powinny być budowane na "mocnych cechach". Możesz wykorzystać do tego wnioski z rozwiązań zadań z poprzednich zajęć.

    -   Podejście z wykorzystaniem modelowania. Za pomocą wybranego modelu podziel sprawy na grupy spraw podobnych. Model zbuduj na zbiorze `cases_train`. Podział na wyznaczone segmenty zaaplikuj do wszystkich zbiorów.
2.  Porównaj oba podejścia w kontekście, czy sprawa płaciła w pierwszych 12 miesiącach obsługi. Samodzielnie wybierz metodę, w jaki sposób je zestawić. Porównania dokonaj na danych ze zbioru `valid`. Do dalszych analiz wybierz lepszą segmentację.

## Zadanie #2

3.  Użyj modelu k najbliższych sąsiadów do rozwiązania poniższych problemów.

    -   *regresja.* Dla spraw ze zbioru `test` wyznacz prognozę wpłat do 12 miesiąca obsługi.

    -   klasyfikacja. Dla spraw ze zbioru `test` zaprognozuj, czy dokonają wpłaty w pierwszych 12 miesiącach obsługi.

    Przetestuj różne warianty hiperparametrów modelu np.: liczba dobieranych sąsiadów, liczba cech oraz jakie cechy wyznaczają przestrzeń, w której wyznaczamy odległości między sprawami.

    Dobieraj sprawy podobne w obrębie wyznaczonych i wybranych segmentów.

    Dobieraj z sąsiadów z połączonych zbiorów `train` i `valid`.

    Wybierz odpowiednie metryki do oceny modeli.
    
