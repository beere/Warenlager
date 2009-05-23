with Ada.Text_IO, Ada.Strings.Unbounded, Ada.Strings,
Ada.Strings.Unbounded.Text_IO, Ada.Integer_Text_IO,
Ada.Float_Text_IO, Listen_Paket;
use Ada.Text_IO, Ada.Strings, Ada.Strings.Unbounded,
Ada.Strings.Unbounded.Text_IO, Ada.Integer_Text_IO, Ada.Float_Text_IO;

package Warenlager is
   Kapazitaet, Schutzzeit, Passwort : Integer := 0;
   Durch_Umsatz : Float := 0.0;
   ----------------------------------------------------------------------------
   --  Artikel
   type Artikel;
   type Ref_Artikel is access Artikel;
   type Artikel is record 
      Artiklelnummer : Integer;
      Artikelname : Unbounded_String;
      Anzahl : Integer;
      Verkauft : Integer;
      Mindeststueckzahl : Integer;
      Prioritaet : Float;
      Preis : Float;
      Datum : String (1 .. 8);
   end record;
   function "<" (A : Ref_Artikel; B : Ref_Artikel) return Boolean;
   package Artikel_Liste is new Listen_Paket (Ref_Artikel, "<");
   Artikelliste : Artikel_Liste.Liste;
   package Bestell_Liste is new Listen_Paket (Ref_Artikel, "<");
   Bestellliste : Bestell_Liste.Liste;
   ----------------------------------------------------------------------------
   --  Auftrag
   type Auftrag;
   type Ref_Auftrag is access Auftrag;
   type Auftrag is record
      Kunde : Ref_Kunde;
      Auftragsnummer : Integer; 
      Artikel : Ref_Artikel;
      Stueckzahl : Integer;
      Prioritaet : Float;
   end record;
   function "<" (A : Ref_Auftrag; B : Ref_Auftrag) return Boolean;
   package Auftrag_Liste is new Listen_Paket (Ref_Auftrag, "<");
   Auftragsliste : Auftrag_Liste.Liste;
   ----------------------------------------------------------------------------
   --  Kunde
   type Kunde;
   type Ref_Kunde is access Kunde;
   type Kunde is record
      Kundennummer : Integer;
      Name : Unbounded_String;
      Umsatz : Integer; -- evtl.
      Kategorie : Integer;
      Prioritaet : Float;
   end record;
   function "<" (A : Ref_Kunde; B : Ref_Kunde) return Boolean;
   package Kunde_Liste is new Listen_Paket (Ref_Kunde, "<");
   Kundenliste : Kunde_Liste.Liste;
   ----------------------------------------------------------------------------
   --  michael
   procedure Auftraege_anzeigen (L : in out Auftrag_Liste.Liste);
   procedure Neuer_Auftrag (AL : in out Auftrag_Liste.Liste; KL : in out Kunde_Liste.Liste; ARL : in out Artikel_Liste.Liste);
   procedure Auftrag_bearbeiten (L : in out Auftrag_Liste.Liste);
   procedure Auftrag_loeschen (L : in out Auftrag_Liste.Liste);
   procedure Auftraege_Laden (L : in out Auftrag_Liste.Liste);
   procedure Auftraege_Speichern (L : in out Auftrag_Liste.Liste);
   procedure Auftraege_Abarbeiten (L : in out Auftrag_Liste.Liste);
   
   --  denis
   procedure Artikel_Anzeigen (L : in out Artikel_Liste.Liste);
   procedure Neuer_Artikel (L : in out Artikel_Liste.Liste);
   procedure Artikel_bearbeiten (L : in out Artikel_Liste.Liste);
   procedure Artikel_loeschen (L : in out Artikel_Liste.Liste);
   procedure Artikel_Laden (L : in out Artikel_Liste.Liste);
   procedure Artikel_Speichern (L : in out Artikel_Liste.Liste);
   procedure Artikel_Bestellen (A : Ref_Artikel; N : Integer;
   L : Bestell_Liste.Liste);
   procedure Bestellung_Bestaetigen (L : in out Bestell_Liste.Liste);
   procedure Lagerbestand_Ueberpruefen (L : in out Artikel_Liste.Liste);
   procedure Lagerbestand_Anzeigen (L : in out Artikel_Liste.Liste);
   
   --  rudi
   procedure Kunde_Anzeigen (L : in out Kunde_Liste.Liste);
   procedure Neuer_Kunde (L : in out Kunde_Liste.Liste);
   procedure Kunde_bearbeiten (L : in out Kunde_Liste.Liste);
   procedure Kunde_loeschen (L : in out Kunde_Liste.Liste);
   procedure Kunde_Laden (L : in out Kunde_Liste.Liste);
   procedure Kunde_Speichern (L : in out Kunde_Liste.Liste);
   
   --  rudi
   procedure Empfehlung (ARL : Auftrag_Liste.Liste; 
   AUL : Auftrag_Liste.Liste);
   
   --  rudi
   procedure Grundkonf;
   
   --  michael
   procedure Prioriteaten_Berechnen (A : in out Ref_Auftrag; 
   KL : in out Kunde_Liste.Liste; AL : in out Artikel_Liste.Liste);

end Warenlager;