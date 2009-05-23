package body Warenlager is

   function "<" (A : Ref_Artikel; B : Ref_Artikel) return Boolean is
   begin
      return A.Prioritaet > B.Prioritaet;
   end "<";
   
   function "<" (A : Ref_Auftrag; B : Ref_Auftrag) return Boolean is
   begin
      return A.Prioritaet > B.Prioritaet;
   end "<";
   
   function "<" (A : Ref_Kunde; B : Ref_Kunde) return Boolean is
   begin
      return A.Prioritaet > B.Prioritaet;
   end "<";
   
   procedure Grundkonf is
      Warenlagertxt : File_Type;
      Zeile : String (1 .. 255);
      Pass_Temp, Laenge : Integer;
      Str : Unbounded_String;
      C : Character;
      Z : Integer := 0;
   begin
      Put_Line ("**********Warenlager**********");
      Open (Warenlagertxt, In_File, "/media/TREKSTOR/Info/Semester II/" & 
      "Prokurs II/Blatt 2/warenlager/Warenlager.txt");
      if End_Of_File (Warenlagertxt) then
         Close (Warenlagertxt);
         Open (Warenlagertxt, Out_File, "/media/TREKSTOR/Info/Semester II/" & 
         "Prokurs II/Blatt 2/warenlager/Warenlager.txt");
         Put_Line ("Erster Start des Programms..." & 
         "Grundkonfiguration wird benoetigt");
         Put ("Kapazitaet des Warenlagers: ");
         Get (Kapazitaet);
         Put ("Durchschnittlicher Umsatz pro Auftrag(XXX.XX): ");
         Get (Durch_Umsatz);
         Put_Line ("Wie lange soll ein neuer Artikel vor der ");
         Put ("Aussortierungsempfehlung geschuetzt werden?(in Wochen): ");
         Get (Schutzzeit);
         loop
            Put ("Passwort auswaehlen: ");
            Get (Passwort);
            Put ("Passwort wiederholen: ");
            Get (Pass_Temp)
            if Passwort = Pass_temp then
               exit;
            else
               Put_Line ("Eingabe nicht identisch. Neuer Versuch!");
            end if;
         end loop;
         Reset (Warenlagertxt);
         Str := To_Unbounded_String (Integer'Image (Kapazitaet) & 
         Float'Image (Durch_Umsatz) & Integer'Image (Schutzzeit));
         Put_Line (Warenlagertxt, Str);
         Close (Warenlagertxt);
      else
         Get_Line (Warenlagertxt, Zeile, Laenge);
         for I in 1 .. Laenge loop
            loop
               Get (C);
               if C /= ' ' then
                  Kapazitaet := Kapazitaet * 10 + Character'Pos (C) - 48;
               else
                  Get (C);
                  exit;
               end if;
            end loop;
            loop
               Get (C);
               if C /= ' ' then
                  Kapazitaet := Kapazitaet * 10 + Character'Pos (C) - 48;
               else
                  Get (C);
                  exit;
               end if;
            end loop;
         end loop;
      end if;
   end Grundkonf;

   -- #########################################################
   -- 
   -- #########################################################

   --  Abschnitt der Aufträge
   ---------------------------------------------------------------------------
   --  PROCEDURE Auftrag_Ausgeben
   --
   --  Gibt einen Auftrag aus
   ---------------------------------------------------------------------------

   function Auftrag_Ausgeben (A : Auftrag) return String is
      Unb_Str : Unbounded_String;
   begin
      Unb_Str := A.Kundennummer & "," & A.Auftragsnummer & "," & A.Artikelnummer & ","
      & A.Stueckzahl & "," & A.Prioritaet;
      return To_String (Unb_Str);
   end Auftrag_Ausgeben;

   procedure Auftraege_anzeigen (L : in out Auftrag_Liste.Liste) is
      C : Auftrag_Liste.Listen_Cursor;
   begin
      C := Auftrag_Liste.Cursor_Listen_Anfang (L);
      while Auftrag_Liste.Ist_Cursor_Zulaessig (C) loop
         Put_Line (Auftrag_Ausgeben (L.Inhalt_An_Cursor (C)));
         Auftrag_Liste.Gehe_Vorwaerts (C);
      end loop;
   end Auftraege_anzeigen;

   ---------------------------------------------------------------------------
   --  PROCEDURE Neuer_Auftrag
   --  Beere erledigt diesen Part !!!!
   --  Erstellt einen neuen Auftrag und fügt ihn an eine Liste an
   ---------------------------------------------------------------------------

   procedure Neuer_Auftrag (AL : in out Auftrag_Liste.Liste; KL : in out Kunde_Liste.Liste; ARL : in out Artikel_Liste.Liste) is
      Neuer_Auftrag : Auftrag;
      Temp          : Integer;
      C : Kunde_Liste.Listen_Cursor;
      C : Artikel_Liste.Listen_Cursor;
      Temp_Kunde : Kunde;
   begin
      New_Line;
      Put ("Kundennummer: ");
      Get (Temp);
      --  Suchen nach Kundendatensatz
      C := Kunde_Liste.Cursor_Listen_Anfang (KL);
      while Kunde_Liste.Ist_Cursor_Zulaessig (C) loop
         Temp_Kunde := KL.Inhalt_An_Cursor (C);
         if Temp_Kunde.Kundennummer = Temp then
            Neuer_Auftrag.Kunde := Temp_Kunde;
         end if;           
         Kunde_Liste.Gehe_Vorwaerts (C);
      end loop;
      Put ("Auftragsnummer: ");
      Get (Temp);
      Neuer_Auftrag.Auftragsnummer := Temp;
      Put ("Artikelnummer: ");
      Get (Temp);
      C := Artikel_Liste.Cursor_Listen_Anfang (ARL);
      while Artikel_Liste.Ist_Cursor_Zulaessig (C) loop
         Temp_Artikel := Artikel_Liste.Inhalt_An_Cursor (C);
         if Temp_Artikel.Artikel = Temp then
            Neuer_Auftrag.Artikel := Temp_Artikel;
         end if;           
         Artikel_Liste.Gehe_Vorwaerts (C);
      end loop;
      Neuer_Auftrag.Artikelnummer := Temp;
      Put ("Stueckzahl: ");
      Get (Temp);
      Neuer_Auftrag.Stueckzahl := Temp;
      --  Prioritaeten-Berechnung
      Berechne_Prioritaeten (Neuer_Auftrag, KL, ARL);
      if Neuer_Auftrag.Kunde.Kategorie = 1 then 
         Auftrag_Liste.Anhaengen_Hinten (L, Neuer_Auftrag);
      else
         Auftrag_Liste.Sortiert_Einfuegen (AL, Neuer_Auftrag);
      end if;
   end Neuer_Auftrag;


   ---------------------------------------------------------------------------
   --  PROCEDURE Auftrag_bearbeiten
   --  UNFERTIG !!!!
   --  Bearbeitet einzelne Auftrage in einer Liste
   ---------------------------------------------------------------------------

   procedure Auftrag_bearbeiten (L : in out Auftrag_Liste.Liste) is
      Beab_Auftragsnummer : Integer;
      Option              : Natural range 0 .. 4;
      Temp                : Integer;
      C                   : Auftrag_Liste.Listen_Cursor := Auftrag_Liste.Cursor_Listen_Anfang (L);
      Auftrag_Gefunden    : Boolean := False
   begin
      Auftraege_anzeigen (L);
      Put ("Auftragsnummer: ");
      Get (Beab_Auftragsnummer);
      New_Line;
      while Auftrag_Liste.Ist_Cursor_Zulaessig (C) and then (not Auftrag_Gefunden) loop
         if Auftrag_Liste.Inhalt_An_Cursor(C).Auftragsnummer = Beab_Auftragsnummer then
            Auftrag_Gefunden := True;
         end if;
         Auftrag_Liste.Gehe_Vorwaerts (C);
      end loop;
      if Auftrag_Gefunden then
         Auftrag_Liste.Gehe_Rueckwaerts (C);
         Put_Line ("Was möchten Sie bearbeiten?");
         Put ("(1) Kundennummer: ");
         Put ("(2) Auftragsnummer: ");
         Put ("(3) Artikelnummer: ");
         Put ("(4) Stueckzahl: ");
         Put ("(0) Auftrag Bearbeiten schliessen");
         Get (Option);
         New_Line;
         while Option /= 0 loop
            case Option is
               when 1 =>
                  Put ("Kundennummer aendern in: ");
                  Get (Temp);
                  Auftrag_Liste.Inhalt_An_Cursor (C).Kundennummer := Temp;
               when 2 =>
                  Put ("Auftragsnummer aendern in: ");
                  Get (Temp);
                  Auftrag_Liste.Inhalt_An_Cursor (C).Auftragsnummer := Temp;
               when 3 =>
                  Put ("Artikelnummer aendern in: ");
                  Get (Temp);
                  Auftrag_Liste.Inhalt_An_Cursor (C).Artikelnummer := Temp;
               when 4 =>
                  Put ("Stueckzahl aendern in: ");
                  Get (Temp);
                  Auftrag_Liste.Inhalt_An_Cursor (C).Stueckzahl := Temp;
            end case;
         end loop;
      else
         Put_Line ("Auftragsnummer nicht gefunden");
      end if;
      --  Einen neuen Auftrag an Liste nach Prio einfügen und alten Auftrag löschen? !!!
   end Auftrag_bearbeiten;

   ---------------------------------------------------------------------------
   --  PROCEDURE Auftraege_Speichern
   --
   --  Speichert eine Liste in eine Datei
   ---------------------------------------------------------------------------
   procedure Auftraege_Speichern (L : in out Auftrag_Liste.Liste) is
      Datei   : File_Type;
      Unb_Str : Unbounded_String;
      C       : Auftrag_Liste.Listen_Cursor := Auftrag_Liste.Cursor_Listen_Anfang (L);
   begin
      Put_Line ("Geben Sie den neuen Namen der Datei an: ");
      Unb_Str := Get_Line;
      Create (Datei, Out_File, To_String (Unb_Str));
      while Auftrag_Liste.Ist_Cursor_Zulaessig (C) loop
         Put_Line (Datei, Auftrag_Ausgeben (Auftrag_Liste.Inhalt_An_Cursor (C)));
--         Put_Line (Auftrag_Ausgeben (Auftrag_Liste.Inhalt_An_Cursor (C)));
         Auftrag_Liste.Gehe_Vorwaerts (C);
      end loop;
      Close (Datei);
   end Auftraege_Speichern;

   ---------------------------------------------------------------------------
   --  PROCEDURE Auftraege_Laden
   --
   --  Lädt eine Datei in eine Liste
   ---------------------------------------------------------------------------
   procedure Auftraege_Laden (L : in out Auftrag_Liste.Liste) is
      Unb_Str : Unbounded_String;
      Zeile   : String (1 .. 255);
      Laenge  : Natural;
      Datei   : File_Type;
      --  Eigene Prozedur zum einzelne Zeilen aufsplitten und dem Record hinzuzufügen
      procedure Auftrag_Speichern is
         A      : Auftrag;
         K      : Natural := 1;
         Opt    : Natural := 0;
      begin
         for i in 1 .. Laenge loop
            if Zeile (i) = ',' or else i = Laenge then
               Opt := Var + 1;
               case Opt is
                  when 1 => A.Kundennummer   := To_Unbounded_String (Zeile (K .. (I - 1)));
                  when 2 => A.Auftragsnummer := To_Unbounded_String (Zeile (K .. (I - 1)));
                  when 3 => A.Artikelnummer  := To_Unbounded_String (Zeile (K .. (I - 1)));
                  when 4 => A.Artikelnummer  := To_Unbounded_String (Zeile (K .. (I - 1)));
                  when 5 => A.Stueckzahl     := To_Unbounded_String (Zeile (K .. I));
                  when others => null;
               end case;
               K := I + 1;
            end if;
         end loop;
         Auftrag_Liste.Anhaengen_Hinten (L, A);
      end Auftrag_Speichern;
   begin
      L := Auftrag_Liste.Neue_Liste;
      Put_Line ("Geben Sie den Namen der Datei ein: ");
      Unb_Str := Get_Line;
      Open (Datei, In_File, To_String (Unb_Str));
      while not End_Of_File (Datei) loop
         Get_Line (Datei, Zeile, Laenge);
         Auftrag_Speichern;
      end loop;
      Close (Datei);
   end Auftraege_Laden;

   --  Abschnitt der Kunden

   ---------------------------------------------------------------------------
   --  PROCEDURE Kunden_Ausgeben
   --
   --  Gibt einen Kunden aus
   ---------------------------------------------------------------------------

   function Kunde_Ausgeben (A : Kunde) return String is
      Unb_Str : Unbounded_String;
   begin
      Unb_Str := A.Kundennummer & "," & A.Name & "," & A.Umsatz & ","
      & A.Kategorie & "," & A.Prioritaet;
      return To_String (Unb_Str);
   end Kunde_Ausgeben;

   procedure Kunde_Anzeigen (L : in out Kunde_Liste.Liste) is
      C : Kunde_Liste.Listen_Cursor;
   begin
      C := Kunde_Liste.Cursor_Listen_Anfang (L);
      while Kunde_Liste.Ist_Cursor_Zulaessig (C) loop
         Put_Line (Kunde_Ausgeben (L.Inhalt_An_Cursor (C)));
         Kunde_Liste.Gehe_Vorwaerts (C);
      end loop;
   end Kunde_Anzeigen;

   ---------------------------------------------------------------------------
   --  PROCEDURE Neuer_Kunde
   --
   --  Erstellt einen neuen Kunden und fügt ihn an eine Liste an
   ---------------------------------------------------------------------------

   procedure Neuer_Kunde (L : in out Kunde_Liste.Liste) is
      Neu_Kunde : Kunde;
      Temp : Integer;
      Unb_Str : Unbounded_String;
   begin
      New_Line;
      Put ("Kundennummer: ");
      Get (Temp);
      Neu_Kunde.Kundennummer := Temp;
      --  Check, ob Kundennummer ex. hinzufügen !!!
      Put ("Name: ");
      Unb_Str := Get_Line;
      Neu_Kunde.Name := Unb_Str;
      Put ("Kategorie (1 Kleinkunde, 2 Grosskunde): ");
      Get (Temp);
      Neu_Kunde.Kategorie := Temp;
      Neu_Kunde.Prioritaet := 5.0;
      Neu_Kunde.Umsatz := 0;
      Kunde_Liste.Anhaengen_Hinten (L, Neu_Kunde);
   end Neuer_Kunde;

   ---------------------------------------------------------------------------
   --  PROCEDURE Kunde_bearbeiten
   --  UNFERTIG !!!!
   --  Bearbeitet einzelne Kunden in einer Liste
   ---------------------------------------------------------------------------

   procedure Kunde_bearbeiten (L : in out Kunde_Liste.Liste) is
      Beab_Kundennummer : Integer;
      Option  : Natural range 0 .. 4;
      Unb_Str : Unbounded_String;
      Temp : Integer;
      C : Kunde_Liste.Listen_Cursor := Kunde_Liste.Cursor_Listen_Anfang (L);
      Kunde_Gefunden : Boolean := False
   begin
      Auftraege_anzeigen (L);
      Put ("Kundennummer: ");
      Get (Beab_Kundennummer);
      New_Line;
      while Kunde_Liste.Ist_Cursor_Zulaessig (C) and then (not Kunde_Gefunden) loop
         if Kunde_Liste.Inhalt_An_Cursor(C).Kundennummer = Beab_Kundennummer then
            Kunde_Liste := True;
         end if;
         Kunde_Liste.Gehe_Vorwaerts (C);
      end loop;
      if Kunde_Gefunden then
         Kunde_Liste.Gehe_Rueckwaerts (C);
         Put_Line ("Was möchten Sie bearbeiten?");
         Put ("(1) Kundennummer: ");
         Put ("(2) Name: ");
         Put ("(3) Kategorie: ");
         Put ("(0) Kunden Bearbeiten schliessen");
         Get (Option);
         New_Line;
         while Option /= 0 loop
         case Option is
            when 1 =>
               Put ("Kundennummer aendern in: ");
               Get (Temp);
               Kunde_Liste.Inhalt_An_Cursor (C).Kundennummer := Temp;
            when 2 =>
               Put ("Namen aendern in: ");
               Unb_Str := Get_Line;
               Kunde_Liste.Inhalt_An_Cursor (C).Name := Unb_Str;
            when 3 =>
               Put ("Kategorie aendern in (1 Kleinkunde, 2 Großkunde: ");
               Get (Temp);
               Kunde_Liste.Inhalt_An_Cursor (C).Kategorie := Temp;
         end case;
         end loop;
      else
         Put_Line ("Kundennummer nicht gefunden");
      end if;
      --  Einen neuen Auftrag an Liste nach Prio einfügen und alten Auftrag löschen? !!!
   end Kunde_bearbeiten;

   ---------------------------------------------------------------------------
   --  PROCEDURE Kunde_Speichern
   --
   --  Speichert eine Liste in eine Datei
   ---------------------------------------------------------------------------
   procedure Kunde_Speichern (L : in out Kunde_Liste.Liste) is
      Datei   : File_Type;
      Unb_Str : Unbounded_String;
      C       : Kunde_Liste.Listen_Cursor := Kunde_Liste.Cursor_Listen_Anfang (L);
   begin
      Put_Line ("Geben Sie den neuen Namen der Datei an: ");
      Unb_Str := Get_Line;
      Create (Datei, Out_File, To_String (Unb_Str));
      while Kunde_Liste.Ist_Cursor_Zulaessig (C) loop
         Put_Line (Datei, Kunde_Ausgeben (Kunde_Liste.Inhalt_An_Cursor (C)));
--         Put_Line (Kunde_Ausgeben (Kunde_Liste.Inhalt_An_Cursor (C)));
         Kunde_Liste.Gehe_Vorwaerts (C);
      end loop;
      Close (Datei);
   end Kunde_Speichern;

   ---------------------------------------------------------------------------
   --  PROCEDURE Kunde_Laden
   --
   --  Lädt eine Datei in eine Liste
   ---------------------------------------------------------------------------
   procedure Kunde_Laden (L : in out Kunde_Liste.Liste) is
      Unb_Str : Unbounded_String;
      Zeile   : String (1 .. 255);
      Laenge  : Natural;
      Datei   : File_Type;
      --  Eigene Prozedur zum einzelne Zeilen aufsplitten und dem Record hinzuzufügen
      procedure Kunde_Speichern is
         A      : Kunde;
         K      : Natural := 1;
         Opt    : Natural := 0;
      begin
         for i in 1 .. Laenge loop
            if Zeile (i) = ',' or else i = Laenge then
               Opt := Var + 1;
               case Opt is
                  when 1 => A.Kundennummer   := To_Unbounded_String (Zeile (K .. (I - 1)));
                  when 2 => A.Name := To_Unbounded_String (Zeile (K .. (I - 1)));
                  when 3 => A.Umsatz  := To_Unbounded_String (Zeile (K .. (I - 1)));
                  when 4 => A.Kategorie  := To_Unbounded_String (Zeile (K .. (I - 1)));
                  when 5 => A.Prioritaet     := To_Unbounded_String (Zeile (K .. I));
                  when others => null;
               end case;
               K := I + 1;
            end if;
         end loop;
         Kunde_Liste.Anhaengen_Hinten (L, A);
      end Kunde_Speichern;
   begin
      L := Kunde_Liste.Neue_Liste;
      Put_Line ("Geben Sie den Namen der Datei ein: ");
      Unb_Str := Get_Line;
      Open (Datei, In_File, To_String (Unb_Str));
      while not End_Of_File (Datei) loop
         Get_Line (Datei, Zeile, Laenge);
         Kunde_Speichern;
      end loop;
      Close (Datei);
   end Kunde_Laden;

   --  Abschnitt der Artikel

   ---------------------------------------------------------------------------
   --  PROCEDURE Artikel_Ausgeben
   --
   --  Gibt einen Artikel aus
   ---------------------------------------------------------------------------

   function Artikel_Ausgeben (A : Artikel) return String is
      Unb_Str : Unbounded_String;
   begin
      Unb_Str := A.Kundennummer & "," & A.Name & "," & A.Umsatz & ","
      & A.Kategorie & "," & A.Prioritaet;
      return To_String (Unb_Str);
   end Artikel_Ausgeben;

   procedure Artikel_Anzeigen (L : in out Artikel_Liste.Liste) is
      C : Artikel_Liste.Listen_Cursor;
   begin
      C := Artikel_Liste.Cursor_Listen_Anfang (L);
      while Artikel_Liste.Ist_Cursor_Zulaessig (C) loop
         Put_Line (Artikel_Ausgeben (L.Inhalt_An_Cursor (C)));
         Artikel_Liste.Gehe_Vorwaerts (C);
      end loop;
   end Artikel_Anzeigen;

   ---------------------------------------------------------------------------
   --  PROCEDURE Neuer_Artikel
   --
   --  Erstellt einen neuen Artikel und fügt ihn an eine Liste an
   ---------------------------------------------------------------------------

   procedure Neuer_Artikel (L : in out Artikel_Liste.Liste) is
      Neu_Artikel : Artikel;
      Temp : Integer;
      Temp2 : Float;
      Dat : String (1 .. 8);
      Unb_Str : Unbounded_String;
   begin
      New_Line;
      Put ("Artikelnummer: ");
      Get (Temp);
      Neu_Artikel.Artikelnummer := Temp;
      --  Check, ob Kundennummer ex. hinzufügen !!!
      Put ("Artikelname: ");
      Unb_Str := Get_Line;
      Neu_Artikel.Artikelname := Unb_Str;
      Put ("Anzahl Artikel: ");
      Get (Temp);
      Neu_Artikel.Anzahl := Temp;
      Put ("Mindeststueckzahl: ");
      Get (Temp);
      Neu_Artikel.Mindeststueckzahl := Temp;
      Put ("Preis: ");
      Get (Temp2);
      Neu_Artikel.Preis := Temp2;
      Put ("Heutiges Datum (DDMMYYYY): ");
      Get (Dat);
      Neu_Artikel.Datum := Dat;
      Neu_Artikel.Prioritaet := 5.0;
      Neu_Artikel.Verkauft := 0;
      Artikel_Liste.Anhaengen_Hinten (L, Neu_Artikel);
   end Neuer_Artikel;

   ---------------------------------------------------------------------------
   --  PROCEDURE Artikel_bearbeiten
   --  UNFERTIG !!!!
   --  Bearbeitet einzelne Artikel in einer Liste
   ---------------------------------------------------------------------------

   procedure Artikel_bearbeiten (L : in out Artikel_Liste.Liste) is
      Beab_Artikelnummer : Integer;
      Option  : Natural range 0 .. 7;
      Unb_Str : Unbounded_String;
      Temp : Integer;
      Temp2 : Float;
      Dat : String (1 .. 8);
      C : Artikel_Liste.Listen_Cursor := Artikel_Liste.Cursor_Listen_Anfang (L);
      Artikel_Gefunden : Boolean := False
   begin
      Auftraege_anzeigen (L);
      Put ("Artikelnummer: ");
      Get (Beab_Artikelnummer);
      New_Line;
      while Artikel_Liste.Ist_Cursor_Zulaessig (C) and then (not Artikel_Gefunden) loop
         if Artikel_Liste.Inhalt_An_Cursor(C).Artikelnummer = Beab_Artikelnummer then
            Artikel_Liste := True;
         end if;
         Artikel_Liste.Gehe_Vorwaerts (C);
      end loop;
      if Artikel_Gefunden then
         Artikel_Liste.Gehe_Rueckwaerts (C);
         Put_Line ("Was möchten Sie bearbeiten?");
         Put ("(1) Artikelnummer: ");
         Put ("(2) Artikelname: ");
         Put ("(3) Anzahl: ");
         Put ("(4) Verkaufte Artikel: ");
         Put ("(5) Mindeststueckzahl: ");
         Put ("(6) Preis: ");
         Put ("(7) Datum der Aufnahme des Artikels in Katalog: ");
         Put ("(0) Artikel Bearbeiten schliessen");
         Get (Option);
         New_Line;
         while Option /= 0 loop
         case Option is
            when 1 =>
               Put ("Artikelnummer aendern in: ");
               Get (Temp);
               Artikel_Liste.Inhalt_An_Cursor (C).Artikelnummer := Temp;
            when 2 =>
               Put ("Artikelname aendern in: ");
               Unb_Str := Get_Line;
               Artikel_Liste.Inhalt_An_Cursor (C).Artikelname := Unb_Str;
            when 3 =>
               Put ("Anzahl aendern in: ");
               Get (Temp);
               Artikel_Liste.Inhalt_An_Cursor (C).Anzahl := Temp;
            when 4 =>
               Put ("Verkaufte Artikel aendern in: ");
               Get (Temp);
               Artikel_Liste.Inhalt_An_Cursor (C).Verkauft := Temp;
            when 5 =>
               Put ("Mindeststueckzahl aendern in: ");
               Get (Temp);
               Artikel_Liste.Inhalt_An_Cursor (C).Mindeststueckzahl := Temp;
            when 6 =>
               Put ("Preis aendern in: ");
               Get (Temp2);
               Artikel_Liste.Inhalt_An_Cursor (C).Preis := Temp2;
            when 7 =>
               Put ("Datum der Aufnahme des Artikels in Katalog aendern in: ");
               Get (Dat);
               Artikel_Liste.Inhalt_An_Cursor (C).Datum := Dat;
         end case;
         end loop;
      else
         Put_Line ("Artikelnummer nicht gefunden");
      end if;
      --  Einen neuen Artikel an Liste nach Prio einfügen und alten Auftrag löschen? !!!
   end Artikel_bearbeiten;

   ---------------------------------------------------------------------------
   --  PROCEDURE Artikel_Speichern
   --
   --  Speichert eine Liste in eine Datei
   ---------------------------------------------------------------------------
   procedure Artikel_Speichern (L : in out Artikel_Liste.Liste) is
      Datei   : File_Type;
      Unb_Str : Unbounded_String;
      C       : Artikel_Liste.Listen_Cursor := Artikel_Liste.Cursor_Listen_Anfang (L);
   begin
      Put_Line ("Geben Sie den neuen Namen der Datei an: ");
      Unb_Str := Get_Line;
      Create (Datei, Out_File, To_String (Unb_Str));
      while Artikel_Liste.Ist_Cursor_Zulaessig (C) loop
         Put_Line (Datei, Artikel_Ausgeben (Artikel_Liste.Inhalt_An_Cursor (C)));
--         Put_Line (Artikel_Ausgeben (Artikel_Liste.Inhalt_An_Cursor (C)));
         Artikel_Liste.Gehe_Vorwaerts (C);
      end loop;
      Close (Datei);
   end Artikel_Speichern;

   ---------------------------------------------------------------------------
   --  PROCEDURE Artikel_Laden
   --
   --  Lädt eine Datei in eine Liste
   ---------------------------------------------------------------------------
   procedure Artikel_Laden (L : in out Artikel_Liste.Liste) is
      Unb_Str : Unbounded_String;
      Zeile   : String (1 .. 255);
      Laenge  : Natural;
      Datei   : File_Type;
      --  Eigene Prozedur zum einzelne Zeilen aufsplitten und dem Record hinzuzufügen
      procedure Artikel_Speichern is
         A      : Kunde;
         K      : Natural := 1;
         Opt    : Natural := 0;
      begin
         for i in 1 .. Laenge loop
            if Zeile (i) = ',' or else i = Laenge then
               Opt := Var + 1;
               case Opt is
                  when 1 => A.Kundennummer   := To_Unbounded_String (Zeile (K .. (I - 1)));
                  when 2 => A.Name := To_Unbounded_String (Zeile (K .. (I - 1)));
                  when 3 => A.Umsatz  := To_Unbounded_String (Zeile (K .. (I - 1)));
                  when 4 => A.Kategorie  := To_Unbounded_String (Zeile (K .. (I - 1)));
                  when 5 => A.Prioritaet     := To_Unbounded_String (Zeile (K .. I));
                  when others => null;
               end case;
               K := I + 1;
            end if;
         end loop;
         Kunde_Liste.Anhaengen_Hinten (L, A);
      end Artikel_Speichern;
   begin
      L := Artikel_Liste.Neue_Liste;
      Put_Line ("Geben Sie den Namen der Datei ein: ");
      Unb_Str := Get_Line;
      Open (Datei, In_File, To_String (Unb_Str));
      while not End_Of_File (Datei) loop
         Get_Line (Datei, Zeile, Laenge);
         Artikel_Speichern;
      end loop;
      Close (Datei);
   end Artikel_Laden;

   -- #########################################################
   -- 
   -- #########################################################

end Warenlager;