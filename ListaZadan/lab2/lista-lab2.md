## Zadanie #1

Sprawdź poprawność danych w zakresie punktów podanych ze slajdu *poprawność danych*. Na zajęciach wybierz 1-2 punkty.

## Zadanie #2

Zdiagnozuj kolumny, które najlepiej różnicują 12 miesięczną skuteczność. Mogą to być kolumny zarówno z tabeli *cases* jak i *events* (oprócz liczby wpłat). Z Tabeli events weź informacje z wierszy do max 6 miesiąca.

<u>Zadanie dodatkowe</u>

Użyj tych kolumn do zaprezentowania wybranych charakterystyk dla badanych zbiorów danych. Wynikiem powinna być tabela w formie, jak w przykładzie dla kolumny *Product*:

| Product    | NoOfCases | ShareOfCases | Principal | ShareOfPrincipal\* | TOA | ShareOfTOA | AvgDPD | SR6M | SR12M |
|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|
| Loan       |           |              |           |                    |     |            |        |      |       |
| CreditCard |           |              |           |                    |     |            |        |      |       |
| **Total**  |           |              |           |                    |     |            |        |      |       |

\* Udział kapitału liczymy jako Principal/TOA dla danej grupy. Jest to sposób inny niż dla pozostałych kolumn "Share", które obliczamy jako suma dla grupy podzielona przez suma dla całości

## Zadanie #3

Napisz funkcję, która przyjmie jako argumenty (1) wektor oraz (2) metodę przekształcenia danych w tym wektorze. W zależności od wartości z argumentu (2) funkcja zwróci:

-   wektor *zestandaryzowany* (średnia=0, wariancja=1)

-   wektor *znormalizowany* (przekształcenie wartości zmiennej na odcinek \[0, 1\])

-   wektor zlogarytmowany

-   wektor spierwiastkowany

Wyznacz macierz współczynników korelacji między wybraną zmienną o wartościach oryginalnych i jej wartościami przekształconymi. Chodzi o wybrana zmienną numeryczną z tabeli cases.

## Zadanie #4

Wyznacz krzywe skuteczności wyników w horyzoncie 12 miesięcy w rozbiciu na grupy wg grup wyznaczonych w zadaniu #2

Wynik jako tabela np.

| Gender | M1  | M2  | M3  | M4  | M5  | M6  | M7  | M8  | M9  | M10 | M11 | M12 |
|--------|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|
| F/M    |     |     |     |     |     |     |     |     |     |     |     |     |

lub

| Month | Female | Male |
|-------|--------|------|

lub

| Gender | Month | SuccessRate |
|--------|-------|-------------|


## Zadanie #5

Wyniki z zadania 5 przedstaw w formie wykresu / wykresów.
