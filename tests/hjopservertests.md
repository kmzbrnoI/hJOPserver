Testy aplikace hJOPserver
=========================

## Jízdni cesty

1. Postavení jízdní cesty
 - Otestovat všechny podmínky postavení jízdní cesty (vlakové i posunové).
   Vždy narušit jednu podmínku a zkontrolovat, že hJOPserver cestu nepostaví.
2. Rušení jízdní cesty
 - Otestovat, že jízdní cestu lze zrušit
3. Zrušení povolující návěsti kvůli narušení podmínky jízdní cesty
 - Vyzkoušet každou podmínku samostatně (obsazení úseku, ztráta koncové polohy výhybky, ...)
 - Otestovat pád návěstidla.
 - Otestovat nouzové zastavení vlaku.

## Vlaky

1. Vytvoření vlaku
2. Smazání vlaku
2. Rozjetí vlaku před návěstidlem
4. Zastavení vlaku před náěvstidlem
5. Zpomalení vlaku před návěstidlem
6. Zastavení vlaku v zastávce
7. Nouzové zastavení vlaku
8. Předvídaný odjezd

## Výhybky

1. Ruční přestavení výhybky
2. Nouzové přestavení při obsazeném úseku výhybky
3. Zavedení, zrušení štítku
4. Zavedení, zrušení výluky
5. Nouzový závěr

## Úseky

1. Zavedení, zrušení štítku
2. Zavedení, zrušení výluky
3. Test obsazení při obsazeném alespoň jednom vstupu

## IR

1. Otestovat zastavení před návěstidlem při obsazení IR čidla.

## Návěstidlo

1. STUJ
2. AB
3. Přivolávací návěst bez podpory zab. zař.
4. Přivolávací návěsti vlakové cesty
5. Zamknutí návěstidla do polohy stůj

## Přejezd

1. Test indikací přejezdu na základě stavu vstupů
2. Test uzavření až po předzváněcí době
3. Test stavění vlakové cesty s přejezdem
4. Test stavění posunové cesty s přejezdem
5. Test ovládání přejezdu úseky
6. Test ovládání pozitivy
7. Zavedení/zrušení štítku

## Trať

1. Test stavů tratě - obsazenost, závěr
2. Žádost o traťový souhlas
3. Udělení traťového souhlasu
4. Odmítnutí traťového souhlasu
5. Zavedení/zrušení zákazu odjezdu do trati
6. Zavedení/zrušení nouzového závěru úvazky
7. Zavedení/zrušení štítku

## Zámek

1. Test ovládání výhybek
2. Uvolnění klíče

## Další bloky

TODO

## Zesilovače

1. Test indikace výpadku DCC
2. Test indikace zkratu
3. Test indikace výpadku napájení zesilovače
4. Test kombinace události

## Vozidla

1. Vytvoření nového vozidla
  - Kontrola duplicitní adresy.
2. Smazání vozidla
3. Úprava vozidla

## Uživatelé

1. Přihlášení k dopravně
2. Vytvoření uživatele
3. Smazání uživatele
