-- A skeleton of an ADA program for an assignment in programming languages

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;


procedure main is

   ----GLOBAL VARIABLES---

   Number_Of_Producers: constant Integer := 5;
   Number_Of_Assemblies: constant Integer := 5;
   Number_Of_Consumers: constant Integer := 2;
   Max_Goodness_Level: constant Integer := 10;
   Max_Opinion_Level: constant Integer := 100;

   subtype Producer_Type is Integer range 1 .. Number_Of_Producers;
   subtype Assembly_Type is Integer range 1 .. Number_Of_Assemblies;
   subtype Consumer_Type is Integer range 1 .. Number_Of_Consumers;
   subtype Goodness_Type is Integer range 0 .. Max_Goodness_Level;
   subtype Opinion_Type is Integer range 0 .. Max_Opinion_Level;
   subtype Breakfast_Range is Integer range 3 .. 5;
   subtype Lunch_Range is Integer range 1 .. 3;

   --each Producer is assigned a Product that it produces
   Product_Name: constant array (Producer_Type) of String(1 .. 8)
   := ("Burger  ", "Frytki  ", "Nuggetsy", "Napoj   ", "Salatka ");
   --Assembly is a collection of products
   Assembly_Name: constant array (Assembly_Type) of String(1 .. 18)
     := ("Zestaw Happy Meal ", "Zestaw Big Mac    ", "Zestaw Nuggets    ", "Zestaw McMuffin   ", "Zestaw McBreakFast");

   Current_Hour : Integer := 0; --obecna godzina

   ----TASK DECLARATIONS----

   -- Producer produces determined product
   task type Producer is
      entry Start(Product: in Producer_Type; Production_Time: in Integer);
   end Producer;

   -- Consumer gets an arbitrary assembly of several products from the buffer
   -- but he/she orders it randomly
   task type Consumer is
      entry Start(Consumer_Number: in Consumer_Type;
                  Consumption_Time: in Integer);
   end Consumer;

   task Time_Manager is
   end Time_Manager;

   task type Charity_Event is
      entry Start;
   end Charity_Event;

      -- Every assembly has its opinion level
   task type Survey is
      entry Receive_Opinion(Opinion_level: in Integer);
      entry Pass_Result(Survey_result: out Integer);
   end Survey;

    task type Survey_Check is
      entry Start;
   end Survey_Check;

   -- Buffer receives products from Producers and delivers Assemblies to Consumers
   task type Buffer is
      -- Accept a product to the storage (provided there is a room for it)
      entry Take(Product: in Producer_Type; Number: in Integer; Success: out Boolean);
      -- Deliver an assembly (provided there are enough products for it)
      entry Deliver(Assembly: in Assembly_Type; Number: out Integer);
      entry Noble_Gift(Goodness_Level: in Goodness_Type);
      entry Survey_Result;
   end Buffer;

   P: array ( 1 .. Number_Of_Producers ) of Producer;
   K: array ( 1 .. Number_Of_Consumers ) of Consumer;
   B: Buffer;
   E: Charity_Event;
   S: Survey;
   SC: Survey_Check;
   ----TASK DEFINITIONS----

    --Producer--

   task body Producer is
      subtype Production_Time_Range is Integer range 1 .. 3;
      package Random_Production is new Ada.Numerics.Discrete_Random(Production_Time_Range);
      --  random number generator
      G: Random_Production.Generator;
      Producer_Type_Number: Integer;
      Product_Number: Integer;
      Production: Integer;
      Random_Time: Duration;
      Success: Boolean;
   begin
      accept Start(Product: in Producer_Type; Production_Time: in Integer) do
         --  start random number generator
         Random_Production.Reset(G);
         Product_Number := 1;
         Producer_Type_Number := Product;
         Production := Production_Time;
      end Start;
      Put_Line(ESC & "[93m" & "Pracownik McDonald's: Rozpoczeto przygotowywanie " & Product_Name(Producer_Type_Number) & ESC & "[0m");

      loop
         Random_Time := Duration(Random_Production.Random(G));
         delay Random_Time;

         Put_Line(ESC & "[93m" & "Pracownik McDonald's: Ukonczono przygotowywanie " & Product_Name(Producer_Type_Number)
               & ", numer " & Integer'Image(Product_Number) & ESC & "[0m");

        -- B.Take(Producer_Type_Number, Product_Number, Success);
         select
            -- Attempt to add product to buffer
            B.Take(Producer_Type_Number, Product_Number, Success);
            Put_Line(ESC & "[93m" & "Pracownik McDonald's: Produkt gotowy i umieszczony na ladzie" & ESC & "[0m");

         or

            delay 0.5;
            Put_Line(ESC & "[97m" & "Pracownik McDonald's: Oczekuje na wolne miejsce na ladzie serwisowej" & ESC & "[0m");

        end select;

         Product_Number := Product_Number + 1;
      end loop;

   end Producer;


   --Consumer--

   task body Consumer is
      subtype Consumption_Time_Range is Integer range 4 .. 8;
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
      Consumer_Name: constant array (1 .. Number_Of_Consumers)
        of String(1 .. 11)
        := ("Konsument 1", "Konsument 2");
   begin
      accept Start(Consumer_Number: in Consumer_Type;
                   Consumption_Time: in Integer) do
         Random_Consumption.Reset(G);
         Random_Assembly_Breakfast.Reset(GB);
         Random_Assembly_Lunch.Reset(GL);
         Random_Opinion.Reset(GO);
         Consumer_Nb := Consumer_Number;
         Consumption := Consumption_Time;
      end Start;

      Put_Line(ESC & "[96m" & "Klient: " & Consumer_Name(Consumer_Nb) & " rozpoczyna zamowienie" & ESC & "[0m");
      loop
         delay Duration(Random_Consumption.Random(G)); --  simulate consumption

         if Current_Hour < 11 then
            Assembly_Type := Random_Assembly_Breakfast.Random(GB);
         else
            Assembly_Type := Random_Assembly_Lunch.Random(GL);
         end if;

         -- take an assembly for consumption

         select
            B.Deliver(Assembly_Type, Assembly_Number);

            if Assembly_Number = 0 then
               Put_Line(ESC & "[91m" & "Klient: " & Consumer_Name(Consumer_Nb) & " mówi: 'No pieknie, znow brakuje zestawu...''" & ESC & "[0m");
                delay 1.0;
            else
                Put_Line(ESC & "[96m" & "Klient: " & Consumer_Name(Consumer_Nb) & " odbiera zestaw " &
                        Assembly_Name(Assembly_Type) & " numer " & Integer'Image(Assembly_Number) & ESC & "[0m");
               S.Receive_Opinion(Random_Opinion.Random(GO));
            end if;

        or
            -- If no assembly is available, wait before trying again
            delay 0.5;
            Put_Line(ESC & "[96m" & "Klient: Oczekiwanie na mozliwosc odebrania zestawu" & ESC & "[0m");

        end select;
      end loop;
   end Consumer;

   task body Time_Manager is
   begin
      loop

         delay 1.0;
         Current_Hour := (Current_Hour + 1) mod 24;
         Put_Line("Godzina w symulacji: " & Integer'Image(Current_Hour) & ":00");

         if Current_Hour = 0 then
            Put_Line("Rozpoczynamy serwowanie menu sniadaniowego!");
         elsif Current_Hour = 11 then
            Put_Line("Rozpoczynamy serwowanie menu lunchowego!");
         end if;

      end loop;
   end Time_Manager;


   --Buffer--

   task body Buffer is
      Storage_Capacity: constant Integer := 30;
      type Storage_type is array (Producer_Type) of Integer;
      Storage: Storage_type
        := (0, 0, 0, 0, 0);
      Assembly_Content: array(Assembly_Type, Producer_Type) of Integer
        := ((2, 1, 2, 0, 2),
            (1, 2, 0, 1, 0),
            (3, 2, 2, 0, 1),
            (1, 0, 0, 1, 1),
            (0, 1, 1, 1, 2));
      Max_Assembly_Content: array(Producer_Type) of Integer;
      Assembly_Number: array(Assembly_Type) of Integer
        := (1, 1, 1, 1, 1);
      In_Storage: Integer := 0;
      Survey_Score: Integer := 0;
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

      function Can_Accept(Product: Producer_Type) return Boolean is
      begin
         if In_Storage >= Storage_Capacity then
            return False;
         else
            return True;
         end if;
      end Can_Accept;

      function Can_Deliver(Assembly: Assembly_Type) return Boolean is
      begin
         for W in Producer_Type loop
            if Storage(W) < Assembly_Content(Assembly, W) then
               return False;
            end if;
         end loop;
         return True;
      end Can_Deliver;

      procedure Storage_Contents is
      begin
         for W in Producer_Type loop
            Put_Line("|   Zawartosc magazynu: " & Integer'Image(Storage(W)) & " " & Product_Name(W));
         end loop;
         Put_Line("|   Laczna liczba produktów w magazynie: " & Integer'Image(In_Storage));

      end Storage_Contents;

      procedure Complete_Noble_Gif is
         begin
         for W in Producer_Type loop
               Storage(W) := Storage(W) / 2;

               if Storage(W) = 1 then
                  Storage(W) := 1;
               elsif Storage(W) = 0 then
                  Storage(W) := 0;

               end if;
            end loop;

            In_Storage := 0;
            for W in Producer_Type loop
               In_Storage := In_Storage + Storage(W);
            end loop;
      end Complete_Noble_Gif;

   begin
      Put_Line(ESC & "[91m" & "Stanowisko Serwisowe: Start" & ESC & "[0m");
      Setup_Variables;
      loop
         select
            accept Deliver(Assembly: in Assembly_Type; Number: out Integer) do

                  if Can_Deliver(Assembly) then
                     Put_Line(ESC & "[91m" & "Stanowisko Serwisowe: Wydano zestaw " & Assembly_Name(Assembly) & " numer " &
                          Integer'Image(Assembly_Number(Assembly)) & ESC & "[0m");
                     for W in Producer_Type loop
                        Storage(W) := Storage(W) - Assembly_Content(Assembly, W);
                        In_Storage := In_Storage - Assembly_Content(Assembly, W);
                     end loop;
                     Number := Assembly_Number(Assembly);
                     Assembly_Number(Assembly) := Assembly_Number(Assembly) + 1;
                  else
                     Put_Line(ESC & "[91m" & "Stanowisko Serwisowe: Brakuje produktów do zestawu " & Assembly_Name(Assembly) & ESC & "[0m");
                     Number := 0;
                  end if;

            end Deliver;
            Storage_Contents;
         or
            accept Take(Product: in Producer_Type; Number: in Integer; Success: out Boolean) do
               Success := True;
               if Can_Accept(Product) then
                  Put_Line(ESC & "[91m" & "Stanowisko Serwisowe: Przyjeto produkt " & Product_Name(Product) & " numer " & Integer'Image(Number) & ESC & "[0m");
                  Storage(Product) := Storage(Product) + 1;
                  In_Storage := In_Storage + 1;
               else
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
                     Put_Line("Stanowisko Serwisowe: Przepelniony! Usuwam najczesciej przechowywany produkt");
                     Storage(Max_Product) := Storage(Max_Product) - 1;
                     delay 0.5;
                     Storage(Product) := Storage(Product) + 1;
                  delay 0.5;

                  end;
               end if;
            end Take;
            Storage_Contents;
         or
            accept Noble_Gift(Goodness_Level: in Goodness_Type) do
               Put_Line(ESC & "[96m" & "Poziom hojnosci na wydarzeniu charytatywnym: " &
               Integer'Image(Goodness_Level) & ESC & "[0m");
               Put_Line(ESC & "[91m" & "Stanowisko Serwisowe: Zrealizowano szlachetna darowizne" & ESC & "[0m");
               Complete_Noble_Gif;
               Storage_Contents;
            end Noble_Gift;
         or
              accept Survey_Result do
               S.Pass_Result(Survey_Score);
               Put_Line(ESC & "[95m" & "Ankieta: Wynik naszej ankiety satysfakcji klienta to " & Integer'Image(Survey_Score) & " / 100" & ESC & "[0m");
            end Survey_Result;
         end select;

      end loop;
   end Buffer;

   --Charity--

   task body Charity_Event is
      subtype Goodness_Of_Heart_Level_Range is Integer range 0 .. 10;
      package Random_Goodness_Level is new Ada.Numerics.Discrete_Random(Goodness_Type);
      G: Random_Goodness_Level.Generator;
      Goodness_Type: Integer;
   begin
      accept Start do
         Random_Goodness_Level.Reset(G);
      end Start;

      loop
         delay 5.0;
         Goodness_Type := Random_Goodness_Level.Random(G);
         if Goodness_Type > 7 then
            B.Noble_Gift(Goodness_Type);
         else
            Put_Line(ESC & "[92m" & "Poziom zaangazowania w akcje charytatywna: " & Integer'Image(Goodness_Type) & ESC & "[0m");
         end if;
     end loop;
   end Charity_Event;

      --Survey--

   task body Survey is
      Opinion_amount: Integer := 0;
      Opinion_score_sum: Integer := 0;
   begin
      loop
         select
            accept Receive_Opinion (Opinion_level : in Integer) do
               Opinion_amount := Opinion_amount + 1;
               Opinion_score_sum := Opinion_score_sum + Opinion_level;
            end Receive_Opinion;
         or
            accept Pass_Result (Survey_result : out Integer) do
               if Opinion_amount /= 0 then
                  Survey_result := Opinion_score_sum / Opinion_amount;
                  end if;
            end Pass_Result;
         end select;
         end loop;
   end Survey;

   task body Survey_Check is
      subtype Survey_Time_Range is Integer range 4 .. 6;
      package Random_Survey_Result is new
        Ada.Numerics.Discrete_Random(Survey_Time_Range);
      GR: Random_Survey_Result.Generator;
   begin
      Put_Line(ESC & "[95m" & "Ankieta: Rozpoczeto badanie satysfakcji klientow" & ESC & "[0m");
      accept Start do
         Random_Survey_Result.Reset(GR);
      end Start;
      loop
         delay(Duration(Random_Survey_Result.Random(GR)));
         B.Survey_Result;
      end loop;
   end Survey_Check;


   ---"MAIN" FOR SIMULATION---
begin
   for I in 1 .. Number_Of_Producers loop
      P(I).Start(I, 10);
   end loop;
   for J in 1 .. Number_Of_Consumers loop
      K(J).Start(J,12);
   end loop;
   E.Start;
   SC.Start;
end main;
