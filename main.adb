-- Szkielet programu w jezyku Ada, symulujacego system obslugi w restauracji typu fast food.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

-- main wywoluje producentow(producers), konsumentow(consumers), akcje charytatywna(charity_event) oraz ankiety(survey_check)
-- za pomoca 'Start', time_manager wywoluje sie samoistnie
-- producenci wywoluja Take w buforze
-- konsumenci w zaleznosci od godziny(time_manager) dostaja inny zestaw, otrzymuja go(lub nie) za pomoca wywolania Deliver w buforze
-- w przypadku otrzymania zestawu, konsument wywoluje Receive_Opinion w tasku Survey i przekazuje losowa opinie z zakresu <0, 100>
-- task charity_event generuje losowa liczbe z zakresu <0,10>, w przypadku liczby > 7, wywoluje noble_gift w buforze
-- noble_gift wywoluje procedure complete_noble_gift znajdujaca sie w buforze, ktora zmniejsza liczbe kazdego produktu o polowe
-- task time_manager w petli kontroluje godzine
-- task survey_check po uplynieciu pewnego czasu wywoluje survey_result znajdujace sie w buforze
-- survey_result natomiast wywoluje pass_result znajdujace w tasku survey_check i zwraca srednia opinii klientow do zmiennej w buforze

procedure main is

   ----GLOBAL VARIABLES----

   -- Liczba producentow
   Number_Of_Producers: constant Integer := 5;
   -- Liczba zestawow
   Number_Of_Assemblies: constant Integer := 5;
   -- Liczba konsumentow
   Number_Of_Consumers: constant Integer := 2;
   -- Maksymalny poziom hojnosci w akcji charytatywnej
   Max_Goodness_Level: constant Integer := 10;
   -- Maksymalna ocena klienta
   Max_Opinion_Level: constant Integer := 100;

   -- Definicje podtypow producenta, zestawu, konsumenta, jakosci oraz opinii.
   subtype Producer_Type is Integer range 1 .. Number_Of_Producers;
   subtype Assembly_Type is Integer range 1 .. Number_Of_Assemblies;
   subtype Consumer_Type is Integer range 1 .. Number_Of_Consumers;
   subtype Goodness_Type is Integer range 0 .. Max_Goodness_Level;
   subtype Opinion_Type is Integer range 0 .. Max_Opinion_Level;
   subtype Breakfast_Range is Integer range 3 .. 5; -- Zakres godzin sniadaniowych
   subtype Lunch_Range is Integer range 1 .. 3;     -- Zakres godzin lunchowych

   -- Kazdy producent przypisany jest do okreslonego produktu
   Product_Name: constant array (Producer_Type) of String(1 .. 8)
     := ("Burger  ", "Frytki  ", "Nuggetsy", "Napoj   ", "Salatka "); --zad 1.
   -- Zestaw to kolekcja produktow
   Assembly_Name: constant array (Assembly_Type) of String(1 .. 18)
     := ("Zestaw Happy Meal ", "Zestaw Big Mac    ", "Zestaw Nuggets    ", "Zestaw McMuffin   ", "Zestaw McBreakFast");

   -- Zmienna przechowujaca aktualna godzine symulacji
   Current_Hour : Integer := 0; --obecna godzina

   ----TASK DECLARATIONS----

   -- Producent produkuje okreslony produkt
   task type Producer is
      entry Start(Product: in Producer_Type; Production_Time: in Integer);
   end Producer;

   -- Konsument zamawia losowy zestaw produktow z bufora
   -- Zamowienie jest realizowane losowo
   task type Consumer is
      entry Start(Consumer_Number: in Consumer_Type;
                  Consumption_Time: in Integer);
   end Consumer;

   task Time_Manager is
   end Time_Manager;

   -- Zadanie odpowiedzialne za wydarzenie charytatywne
   task type Charity_Event is
      entry Start;
   end Charity_Event;

   -- Kazdy zestaw ma przypisana ocene
   task type Survey is
      entry Receive_Opinion(Opinion_level: in Integer);
      entry Pass_Result(Survey_result: out Integer);
   end Survey;

    task type Survey_Check is
      entry Start;
   end Survey_Check;

   -- Bufor odbiera produkty od producentow i dostarcza zestawy do konsumentow
   task type Buffer is
      -- Przyjmij produkt do magazynu (jesli jest na niego miejsce)
      entry Take(Product: in Producer_Type; Number: in Integer; Success: out Boolean);
      -- Dostarcz zestaw (jesli sa dostepne wystarczajace produkty)
      entry Deliver(Assembly: in Assembly_Type; Number: out Integer);
      -- Szlachetna darowizna, zmniejszajaca ilosc produktow
      entry Noble_Gift(Goodness_Level: in Goodness_Type);
      -- Wynik ankiety
      entry Survey_Result;
   end Buffer;

   -- Tablice przechowujace taski dla producentow, konsumentow, bufora, eventu i ankiet
   P: array ( 1 .. Number_Of_Producers ) of Producer;
   K: array ( 1 .. Number_Of_Consumers ) of Consumer;
   B: Buffer;
   E: Charity_Event;
   S: Survey;
   SC: Survey_Check;

   ----TASK DEFINITIONS----

    --Producer--

   -- Task produkujacy produkty
   task body Producer is
      subtype Production_Time_Range is Integer range 1 .. 3;  -- Zakres czasu produkcji
      package Random_Production is new Ada.Numerics.Discrete_Random(Production_Time_Range);
      -- Generator liczb losowych dla czasu produkcji
      G: Random_Production.Generator;
      Producer_Type_Number: Integer;
      Product_Number: Integer;
      Production: Integer;
      Random_Time: Duration;
      Success: Boolean;
   begin
      -- Akceptacja wejscia producenta
      accept Start(Product: in Producer_Type; Production_Time: in Integer) do
         -- Uruchomienie generatora losowego czasu produkcji
         Random_Production.Reset(G);
         Product_Number := 1;
         Producer_Type_Number := Product;
         Production := Production_Time;
      end Start;

      -- Komunikat o rozpoczeciu produkcji produktu
      Put_Line(ESC & "[93m" & "Pracownik McDonald's: Rozpoczeto przygotowywanie " & Product_Name(Producer_Type_Number) & ESC & "[0m");

      -- Petla produkcyjna
      loop
         Random_Time := Duration(Random_Production.Random(G));  -- Losowanie czasu produkcji
         delay Random_Time;  -- Opoznienie symulujace czas produkcji

         -- Komunikat o zakonczeniu produkcji produktu
         Put_Line(ESC & "[93m" & "Pracownik McDonald's: Ukonczono przygotowywanie " & Product_Name(Producer_Type_Number)
               & ", numer " & Integer'Image(Product_Number) & ESC & "[0m");

         -- Przekazanie produktu do bufora
         select
            -- Proba dodania produktu do bufora
            B.Take(Producer_Type_Number, Product_Number, Success);
            Put_Line(ESC & "[93m" & "Pracownik McDonald's: Produkt gotowy i umieszczony na ladzie" & ESC & "[0m");

         or
            -- Jesli nie ma miejsca w buforze, oczekiwanie
            delay 0.5;
            Put_Line(ESC & "[97m" & "Pracownik McDonald's: Oczekuje na wolne miejsce na ladzie serwisowej" & ESC & "[0m");

        end select;

         Product_Number := Product_Number + 1;  -- Zwiekszenie numeru produktu
      end loop;

   end Producer;

   --Consumer--

   -- Task obslugujacy konsumentow
   task body Consumer is
      subtype Consumption_Time_Range is Integer range 4 .. 8;  -- Zakres czasu konsumpcji
      package Random_Consumption is new
        Ada.Numerics.Discrete_Random(Consumption_Time_Range);

      package Random_Opinion is new
        Ada.Numerics.Discrete_Random(Opinion_Type);

      package Random_Assembly_Breakfast is new Ada.Numerics.Discrete_Random(Breakfast_Range);
      package Random_Assembly_Lunch is new Ada.Numerics.Discrete_Random(Lunch_Range);

      G: Random_Consumption.Generator;
      GB: Random_Assembly_Breakfast.Generator;
      GL: Random_Assembly_Lunch.Generator;
      GO: Random_Opinion.Generator;
      Consumer_Nb: Consumer_Type;
      Assembly_Number: Integer;
      Consumption: Integer;
      Assembly_Type: Integer;

      -- Nazwy konsumentow
      Consumer_Name: constant array (1 .. Number_Of_Consumers)
        of String(1 .. 11)
        := ("Konsument 1", "Konsument 2");

   begin
      -- Akceptacja konsumenta i przypisanie losowych generatorow
      accept Start(Consumer_Number: in Consumer_Type;
                   Consumption_Time: in Integer) do
         Random_Consumption.Reset(G);
         Random_Assembly_Breakfast.Reset(GB);
         Random_Assembly_Lunch.Reset(GL);
         Random_Opinion.Reset(GO);
         Consumer_Nb := Consumer_Number;
         Consumption := Consumption_Time;
      end Start;

      -- Komunikat o rozpoczeciu zamowienia
      Put_Line(ESC & "[96m" & "Klient: " & Consumer_Name(Consumer_Nb) & " rozpoczyna zamowienie" & ESC & "[0m");

      loop
         -- Symulacja czasu konsumpcji
         delay Duration(Random_Consumption.Random(G));

         -- Wybor zestawu w zaleznosci od godziny
         if Current_Hour < 11 then
            Assembly_Type := Random_Assembly_Breakfast.Random(GB);  -- Losowanie zestawu sniadaniowego
         else
            Assembly_Type := Random_Assembly_Lunch.Random(GL);      -- Losowanie zestawu lunchowego
         end if;

         -- Proba odebrania zestawu
         select
            -- Odbior zestawu z bufora
            B.Deliver(Assembly_Type, Assembly_Number);

            -- Sprawdzenie, czy zestaw jest dostepny
            if Assembly_Number = 0 then
               Put_Line(ESC & "[91m" & "Klient: " & Consumer_Name(Consumer_Nb) & " mowi: 'No pieknie, znow brakuje zestawu...''" & ESC & "[0m");
                delay 1.0;
            else
                Put_Line(ESC & "[96m" & "Klient: " & Consumer_Name(Consumer_Nb) & " odbiera zestaw " &
                        Assembly_Name(Assembly_Type) & " numer " & Integer'Image(Assembly_Number) & ESC & "[0m");
               -- Przekazanie losowej opinii po otrzymaniu zestawu
               S.Receive_Opinion(Random_Opinion.Random(GO));
            end if;

        or
            -- Jesli nie ma zestawu, oczekiwanie przed ponowna proba
            delay 0.5;
            Put_Line(ESC & "[96m" & "Klient: Oczekiwanie na mozliwosc odebrania zestawu" & ESC & "[0m");

        end select;
      end loop;
   end Consumer;

   -- Zarzadzanie czasem w symulacji
   task body Time_Manager is
   begin
      loop
         -- Zegar symulacji, co 1 sekunde przesuwa godzine o 1
         delay 1.0;
         Current_Hour := (Current_Hour + 1) mod 24;  -- Zegar w cyklu 24-godzinnym
         Put_Line("Godzina w symulacji: " & Integer'Image(Current_Hour) & ":00");

         -- Informowanie o zmianie menu
         if Current_Hour = 0 then
            Put_Line("Rozpoczynamy serwowanie menu sniadaniowego!");
         elsif Current_Hour = 11 then
            Put_Line("Rozpoczynamy serwowanie menu lunchowego!");
         end if;
      end loop;
   end Time_Manager;

   -- Bufor do przechowywania produktow i dostarczania zestawow
   task body Buffer is
      Storage_Capacity: constant Integer := 30;  -- Pojemnosc magazynu
      type Storage_type is array (Producer_Type) of Integer;
      Storage: Storage_type
        := (0, 0, 0, 0, 0);  -- Poczatkowa ilosc produktow w magazynie

      -- Zawartosc kazdego zestawu
      Assembly_Content: array(Assembly_Type, Producer_Type) of Integer
        := ((2, 1, 2, 0, 2),
            (1, 2, 0, 1, 0),
            (3, 2, 2, 0, 1),
            (1, 0, 0, 1, 1), -- dodane 2 nowe zestawy do 'skonstruowania' na potrzeby tematu
            (0, 1, 1, 1, 2));

      -- Najwieksza liczba produktow, jaka moze byc czescia zestawu
      Max_Assembly_Content: array(Producer_Type) of Integer;
      -- Numer kazdego zestawu
      Assembly_Number: array(Assembly_Type) of Integer
        := (1, 1, 1, 1, 1);

      -- Aktualna liczba produktow w magazynie
      In_Storage: Integer := 0;
      Survey_Score: Integer := 0;

      -- Procedura inicjujaca zmienne dla zawartosci zestawow
      procedure Setup_Variables is
      begin
         for W in Producer_Type loop
            Max_Assembly_Content(W) := 0;
            for Z in Assembly_Type loop
               if Assembly_Content(Z, W) > Max_Assembly_Content(W) then
                  Max_Assembly_Content(W) := Assembly_Content(Z, W);
               end if;
            end loop;
         end loop;
      end Setup_Variables;

      -- Sprawdzenie, czy mozna przyjac nowy produkt do magazynu
      function Can_Accept(Product: Producer_Type) return Boolean is
      begin
         if In_Storage >= Storage_Capacity then
            return False;
         else
            return True;
         end if;
      end Can_Accept;

            -- Sprawdzenie, czy mozna dostarczyc zestaw (czy sa wystarczajace produkty)
      function Can_Deliver(Assembly: Assembly_Type) return Boolean is
      begin
         -- Sprawdza, czy wszystkie produkty potrzebne do stworzenia zestawu sa dostepne w magazynie
         for W in Producer_Type loop
            if Storage(W) < Assembly_Content(Assembly, W) then
               return False;  -- Jesli ktoregokolwiek produktu brakuje, nie mozna dostarczyc zestawu
            end if;
         end loop;
         return True;  -- Wszystkie produkty sa dostepne, zestaw moze zostac dostarczony
      end Can_Deliver;

      -- Procedura wypisujaca zawartosc magazynu
      procedure Storage_Contents is
      begin
         -- Dla kazdego typu producenta, wypisz ilosc produktow w magazynie
         for W in Producer_Type loop
            Put_Line("|   Zawartosc magazynu: " & Integer'Image(Storage(W)) & " " & Product_Name(W));
         end loop;
         -- Wypisz calkowita ilosc produktow w magazynie
         Put_Line("|   Laczna liczba produktow w magazynie: " & Integer'Image(In_Storage));
      end Storage_Contents;

      -- Procedura odpowiadajaca za zmniejszenie ilosci produktow o polowe w wyniku szlachetnej darowizny
      procedure Complete_Noble_Gif is
      begin
         for W in Producer_Type loop
            -- Zmniejsz liczbe kazdego produktu o polowe
            Storage(W) := Storage(W) / 2;

            -- Zabezpieczenie, aby ilosc produktu nie byla mniejsza niz 0
            if Storage(W) = 1 then
               Storage(W) := 1;
            elsif Storage(W) = 0 then
               Storage(W) := 0;
            end if;
         end loop;

         -- Aktualizacja liczby produktow w magazynie
         In_Storage := 0;
         for W in Producer_Type loop
            In_Storage := In_Storage + Storage(W);
         end loop;
      end Complete_Noble_Gif;

   begin
      -- Inicjalizacja bufora
      Put_Line(ESC & "[91m" & "Stanowisko Serwisowe: Start" & ESC & "[0m");
      Setup_Variables;  -- Inicjalizacja zmiennych zwiazanych z magazynem

      -- Glowna petla bufora
      loop
         select
            -- Akceptacja zamowienia dostarczenia zestawu
            accept Deliver(Assembly: in Assembly_Type; Number: out Integer) do

               -- Sprawdzenie, czy zestaw moze zostac dostarczony
               if Can_Deliver(Assembly) then
                  -- Dostarcz zestaw, jesli sa wszystkie produkty
                  Put_Line(ESC & "[91m" & "Stanowisko Serwisowe: Wydano zestaw " & Assembly_Name(Assembly) & " numer " &
                          Integer'Image(Assembly_Number(Assembly)) & ESC & "[0m");

                  -- Zmniejsz ilosc produktow potrzebnych do stworzenia zestawu w magazynie
                  for W in Producer_Type loop
                     Storage(W) := Storage(W) - Assembly_Content(Assembly, W);
                     In_Storage := In_Storage - Assembly_Content(Assembly, W);
                  end loop;

                  -- Zaktualizuj numer dostarczonego zestawu
                  Number := Assembly_Number(Assembly);
                  Assembly_Number(Assembly) := Assembly_Number(Assembly) + 1;
               else
                  -- Jesli nie ma wystarczajacej liczby produktow, poinformuj o braku
                  Put_Line(ESC & "[91m" & "Stanowisko Serwisowe: Brakuje produktow do zestawu " & Assembly_Name(Assembly) & ESC & "[0m");
                  Number := 0;  -- Zestaw nie zostal dostarczony
               end if;
            end Deliver;

            -- Wypisz zawartosc magazynu
            Storage_Contents;

         or
            -- Akceptacja nowego produktu w buforze
            accept Take(Product: in Producer_Type; Number: in Integer; Success: out Boolean) do
               Success := True;

               -- Sprawdzenie, czy w magazynie jest miejsce na nowy produkt
               if Can_Accept(Product) then
                  -- Jesli jest miejsce, dodaj produkt do magazynu
                  Put_Line(ESC & "[91m" & "Stanowisko Serwisowe: Przyjeto produkt " & Product_Name(Product) & " numer " & Integer'Image(Number) & ESC & "[0m");
                  Storage(Product) := Storage(Product) + 1;
                  In_Storage := In_Storage + 1;
               else
                  -- Jesli nie ma miejsca, usun najczesciej przechowywany produkt
                  declare
                     Max_Product : Producer_Type := Producer_Type'First;
                     Max_Count : Integer := Storage(Max_Product);
                  begin
                     for K in Producer_Type loop
                        if Storage(K) > Max_Count then
                           Max_Product := K;
                           Max_Count := Storage(K);
                        end if;
                     end loop;

                     -- Usun najczesciej przechowywany produkt z magazynu
                     Put_Line("Stanowisko Serwisowe: Przepelniony! Usuwam najczesciej przechowywany produkt");
                     Storage(Max_Product) := Storage(Max_Product) - 1;
                     delay 0.5;  -- Oczekiwanie na usuniecie produktu
                     Storage(Product) := Storage(Product) + 1;  -- Dodanie nowego produktu do magazynu
                     delay 0.5;
                  end;
               end if;
            end Take;

            -- Wypisz zawartosc magazynu
            Storage_Contents;

         or
            -- Akceptacja darowizny szlachetnej (zmniejszenie ilosci produktow o polowe)
            accept Noble_Gift(Goodness_Level: in Goodness_Type) do
               Put_Line(ESC & "[96m" & "Poziom hojnosci na wydarzeniu charytatywnym: " &
               Integer'Image(Goodness_Level) & ESC & "[0m");
               Put_Line(ESC & "[91m" & "Stanowisko Serwisowe: Zrealizowano szlachetna darowizne" & ESC & "[0m");
               Complete_Noble_Gif;  -- Zmniejszenie ilosci produktow o polowe
               Storage_Contents;    -- Wypisanie aktualnej zawartosci magazynu
            end Noble_Gift;

         or
            -- Wynik ankiety klienta
            accept Survey_Result do
               S.Pass_Result(Survey_Score);  -- Wywolanie pass_result w tasku Survey i aktualizacja wyniku ankiety
               Put_Line(ESC & "[95m" & "Ankieta: Wynik naszej ankiety satysfakcji klienta to " & Integer'Image(Survey_Score) & " / 100" & ESC & "[0m");
            end Survey_Result;
         end select;

      end loop;
   end Buffer;

   --Charity--

   -- Task odpowiedzialny za akcje charytatywna
   task body Charity_Event is
      subtype Goodness_Of_Heart_Level_Range is Integer range 0 .. 10;
      package Random_Goodness_Level is new Ada.Numerics.Discrete_Random(Goodness_Type);
      G: Random_Goodness_Level.Generator;
      Goodness_Type: Integer;
   begin
      -- Rozpoczecie wydarzenia charytatywnego
      accept Start do
         Random_Goodness_Level.Reset(G);  -- Reset generatora liczb losowych dla poziomu hojnosci
      end Start;

      loop
         -- Oczekiwanie przed wygenerowaniem nowego poziomu hojnosci
         delay 5.0;
         Goodness_Type := Random_Goodness_Level.Random(G);

         -- Jesli poziom hojnosci jest wiekszy niz 7, uruchom szlachetna darowizne
         if Goodness_Type > 7 then
            B.Noble_Gift(Goodness_Type);
         else
            -- W przeciwnym razie, wypisz aktualny poziom zaangazowania
            Put_Line(ESC & "[92m" & "Poziom zaangazowania w akcje charytatywna: " & Integer'Image(Goodness_Type) & ESC & "[0m");
         end if;
      end loop;
   end Charity_Event;

   --Survey--

   -- Task odpowiedzialny za obsluge ankiet
   task body Survey is
      Opinion_amount: Integer := 0;        -- Liczba zebranych opinii
      Opinion_score_sum: Integer := 0;     -- Suma wszystkich ocen
   begin
      loop
         select
            -- Akceptacja nowej opinii klienta
            accept Receive_Opinion (Opinion_level : in Integer) do
               Opinion_amount := Opinion_amount + 1;          -- Zwiekszenie liczby opinii
               Opinion_score_sum := Opinion_score_sum + Opinion_level;  -- Dodanie oceny do sumy
            end Receive_Opinion;

         or
            -- Przekazanie wyniku ankiety
            accept Pass_Result (Survey_result : out Integer) do
               if Opinion_amount /= 0 then  -- Sprawdzenie, czy zebralismy opinie
                  Survey_result := Opinion_score_sum / Opinion_amount;  -- Obliczenie sredniej oceny
               end if;
            end Pass_Result;
         end select;
      end loop;
   end Survey;

   --Survey_Check--

   -- Task kontrolujacy czas przeprowadzania ankiet
   task body Survey_Check is
      subtype Survey_Time_Range is Integer range 4 .. 6;
      package Random_Survey_Result is new Ada.Numerics.Discrete_Random(Survey_Time_Range);
      GR: Random_Survey_Result.Generator;
   begin
      Put_Line(ESC & "[95m" & "Ankieta: Rozpoczeto badanie satysfakcji klientow" & ESC & "[0m");

      -- Rozpoczecie zadania ankiety
      accept Start do
         Random_Survey_Result.Reset(GR);  -- Uruchomienie generatora losowych czasow dla ankiety
      end Start;

      loop
         -- Oczekiwanie losowego czasu (4-6 sekund) przed uruchomieniem wyniku ankiety
         delay(Duration(Random_Survey_Result.Random(GR)));
         B.Survey_Result;  -- Wywolanie Survey_Result w tasku Buffer
      end loop;
   end Survey_Check;

   ---"MAIN" FOR SIMULATION---
begin
   -- Uruchomienie producentow
   for I in 1 .. Number_Of_Producers loop
      P(I).Start(I, 10);
   end loop;

   -- Uruchomienie konsumentow
   for J in 1 .. Number_Of_Consumers loop
      K(J).Start(J,12);
   end loop;

   -- Uruchomienie eventu charytatywnego
   E.Start;

   -- Uruchomienie ankiety
   SC.Start;
end main;
