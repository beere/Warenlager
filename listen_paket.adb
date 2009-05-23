--  FILE:    listen_paket.adb
--
--  PROJECT: Programmieruebungen, Uebungsblatt 7
--  VERSION: $Revision: 1 $
--  AUTHOR:  $Author: Sascha Riexinger $
--
-------------------------------------------------------------------------------
--
--  PACKAGE Listen_Paket
--
--  Generisches Listenpaket für doppeltverkettete Listen
--
with Ada.Unchecked_Deallocation;
package body Listen_Paket is

   procedure Free is new Ada.Unchecked_Deallocation (Element, Ref_Element);

   function Neue_Liste return Liste is
      L : Liste;
   begin
      L.Anfang := null;
      L.Ende := null;
      return L;
   end Neue_Liste;


   procedure Anhaengen_Hinten (L : in out Liste; Inhalt : Inhalts_Typ) is
      X : Ref_Element;
   begin
      X := new Element;
      X.Inhalt := Inhalt;
      if L.Ende /= null then
         L.Ende.Nach := X;
         X.Vor := L.Ende;
         L.Ende := X;
      else
         L.Anfang := X;
         L.Ende := X;
      end if;
   end Anhaengen_Hinten;

   procedure Anhaengen_Vorne (L : in out Liste; Inhalt : Inhalts_Typ) is
      X : Ref_Element;
   begin
      X := new Element;
      X.Inhalt := Inhalt;
      if L.Anfang /= null then
         L.Anfang.Vor := X;
         X.Nach := L.Anfang;
         L.Anfang := X;
      else
         L.Ende := X;
         L.Anfang := X;
      end if;
   end Anhaengen_Vorne;


   procedure Sortiert_Einfuegen (L : in out Liste; Inhalt : Inhalts_Typ) is
      X : Ref_Element;
      Lauf : Ref_Element;
   begin
      X := new Element;
      X.Inhalt := Inhalt;
      --  setze Lauf auf Anfang
      Lauf := L.Anfang;
      --  Wenn erter Eintrag
      --     => setze Anfang/Ende auf X
      if Lauf = null then
         L.Anfang := X;
         L.Ende := X;
      --  sonst
      else
         --  Wenn Lauf einen Nachfolger hat und der Lauf.Inhalt < X.Inhalt
         --  Setze Lauf auf den Nachfolger
         while Lauf.Nach /= null and then Lauf.Inhalt < X.Inhalt loop
            Lauf := Lauf.Nach;
         end loop;
         --  Jetzt ist Lauf.Inhalt > X.Inhalt
         --  oder X.Inhalt ist am groessten
         --
         --  Wenn X.Inhalt < Lauf.Inhalt
         --     => setze X zwischen Lauf.Vor und Lauf
         --        => Lauf.Vor | X | Lauf 
         if X.Inhalt < Lauf.Inhalt then
            X.Nach := Lauf;
            if Lauf.Vor = null then
               Anhaengen_Vorne (L, Inhalt);
            else
               X.Vor := Lauf.Vor;
               Lauf.Vor := X;
               X.Vor.Nach := X;
            end if;
         --  Wenn X.Inhalt am groessten
         --     => Anhaengen_Hinten
         else
            Anhaengen_Hinten (L, Inhalt);
         end if;
      end if;
   end Sortiert_Einfuegen;


   procedure Bearbeite_Elemente (L : in out Liste;
      Bearbeitung : Elemente_Bearbeitung) is
      Lauf : Ref_Element;
   begin
      Lauf := L.Anfang;
      while Lauf /= null loop
         Bearbeitung (Lauf.Inhalt);
         Lauf := Lauf.Nach;
      end loop;
   end Bearbeite_Elemente;
   
   procedure Delete_Ende (L : in out Liste)
   is
   begin
      if L.Ende.Vor /= null then
         L.Ende.Vor.Nach := L.Ende;
         L.Ende := L.Ende.Vor;
         Free (L.Ende.Nach);
      else
         Free (L.Ende);
         Free (L.Anfang);
      end if;
   end Delete_Ende;
   
   function Get_Ende (L : in Liste) return Inhalts_Typ
   is
   begin
      return L.Ende.Inhalt;
   end Get_Ende;
   
   procedure Element_Suchen (L : in out Liste; I : in Inhalts_Typ; 
   Zeiger_Auf_Gefundenes_Element : in out Inhalts_Typ) 
   is
      Lauf : Ref_Element;
      Gefunden : Boolean := False;
   begin
      if L.Anfang /= null then
         Lauf := L.Anfang;
         while not Gefunden loop
            if Lauf.Inhalt = I then
               Gefunden := True;
               Zeiger_Auf_Gefundenes_Element := Lauf.Inhalt;
               exit;
            end if;
            if Lauf.Nach /= null then
               Lauf := Lauf.Nach;
            else 
               exit;
            end if;
         end loop;
      end if;
   end Element_Suchen;

   function Cursor_Listen_Anfang (L : in Liste)
      return Listen_Cursor
   is
      LC : Listen_Cursor;
   begin
      LC.Cursor := L.Anfang;
      return LC;
   end Cursor_Listen_Anfang;
   
   function Cursor_Listen_Ende (L : in Liste)
      return Listen_Cursor
   is
      LC : Listen_Cursor;
   begin
      LC.Cursor := L.Ende;
      return LC;
   end Cursor_Listen_Ende;
      
   procedure Gehe_Vorwaerts (C : in out Listen_Cursor)
   is
   begin
      C.Cursor := C.Cursor.Nach;
   end Gehe_Vorwaerts;
   
   procedure Gehe_Rueckwaerts (C : in out Listen_Cursor)
   is
   begin
      C.Cursor := C.Cursor.Vor;
   end Gehe_Rueckwaerts;
   
   function Ist_Cursor_Zulaessig (C : in Listen_Cursor) return Boolean
   is
   begin
      return C.Cursor /= null;
   end Ist_Cursor_Zulaessig;
   
   function Inhalt_An_Cursor (C : in Listen_Cursor) return Inhalts_Typ
   is
   begin
      return C.Cursor.Inhalt;
   end Inhalt_An_Cursor;
   
   procedure Elemente_Loeschen (L : in out Liste; I : Inhalts_Typ)
   is
      Lauf : Ref_Element;
      Hilf : Ref_Element;
   begin
      Lauf := L.Anfang;
      Hilf := new Element;
      if Lauf.Nach = null and Lauf.Inhalt = I then
         L := Neue_Liste;
      end if;
      while Lauf /= null loop
         if Lauf.Inhalt = I then
            if Lauf = L.Anfang then
               L.Anfang := Lauf.Nach;
               Free (Lauf);
               Lauf := L.Anfang;
               Lauf.Vor := null;
            elsif Lauf = L.Ende then
               L.Ende := Lauf.Vor;
               Free (Lauf);
               Lauf := L.Ende;
               Lauf.Nach := null;
            else
               Lauf.Vor.Nach := Lauf.Nach;
               Lauf.Nach.Vor := Lauf.Vor;
               Hilf := Lauf.Nach;
               Free (Lauf);
               Lauf := Hilf;
            end if;
         else
            Lauf := Lauf.Nach;
         end if;
      end loop;
      if L.Anfang = null then
         L.Anfang := Hilf;
      elsif L.Ende = null then
         L.Ende := Hilf;
      end if;
   end Elemente_Loeschen;
   
   procedure Vertausche (R1 : Ref_Element; R2 : Ref_Element)
   is
      Help : Ref_Element;
   begin
      Help := new Element;
      Help.Inhalt := R1.Inhalt;
      R1.Inhalt := R2.Inhalt;
      R2.Inhalt := Help.Inhalt;
   end Vertausche;
   
   procedure Liste_Sortieren (L : in out Liste; F : in Funktion)
   is
      Sortiert : Boolean := True;
      Lauf : Ref_Element;
   begin
      Lauf := L.Anfang;
      loop
         while Lauf.Nach /= null loop
            if  F (Lauf.Inhalt, Lauf.Nach.Inhalt) then
               Vertausche (Lauf, Lauf.Nach);
               Sortiert := False;
            end if;
            Lauf := Lauf.Nach;
         end loop;
         exit when Sortiert;
         Lauf := L.Anfang;
         Sortiert := True;
      end loop;
   end Liste_Sortieren;
   
   procedure Liste_Anhaengen (L1 : in out Liste; L2 : in Liste)
   is
      Lauf : Ref_Element := L2.Anfang;
   begin
      while Lauf /= null loop
         Anhaengen_Hinten (L1, Lauf.Inhalt);
         Lauf := Lauf.Nach;
      end loop;
   end Liste_Anhaengen;
end Listen_Paket;