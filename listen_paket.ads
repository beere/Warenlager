--  FILE:    listen_paket.ads
--
--  PROJECT: Programmieruebungen, Uebungsblatt 7
--  VERSION: $Revision: 1 $
--  AUTHOR:  $Author: Unbekannt $
--
-------------------------------------------------------------------------------
--
--  PACKAGE Listen_Paket
--
--  Generisches Listenpaket für doppeltverkettete Listen
--
generic
   type Inhalts_Typ is private;
   with function "<" (A : Inhalts_Typ; B : Inhalts_Typ) return Boolean;
   --~ with function "=" (A : Inhalts_Typ; B : Inhalts_Typ) return Boolean;

package Listen_Paket is
   type Liste is private;
   type Listen_Cursor is private;
   
   --  FUNCTION Neue_Liste
   --
   --  Erzeugt eine neue, leere Liste.
   function Neue_Liste return Liste;

   --  PROCEDURE Anhaengen_Hinten
   --
   --  Hängt den übergebenen, neuen Datensatz an eine ebenfalls zu
   --  übergebende Liste HINTEN an.
   --
   --  PARAMETERS:
   --  + L      : Liste an die angehängt werden soll
   --  + Inhalt : Anzuhängender Inhalt
   procedure Anhaengen_Hinten (L : in out Liste; Inhalt : Inhalts_Typ);


   --  PROCEDURE Anhaengen_Vorne
   --
   --  Hängt den übergebenen, neuen Datensatz an eine ebenfalls zu
   --  übergebende Liste VORNE an.
   --
   --  PARAMETERS:
   --  + L      : Liste an die angehängt werden soll
   --  + Inhalt : Anzuhängender Inhalt
   procedure Anhaengen_Vorne (L : in out Liste; Inhalt : Inhalts_Typ);

   --  PROCEDURE Sortiert_Einfuegen
   --
   --  Hängt den übergebenen, neuen Datensatz an eine ebenfalls zu
   --  übergebende Liste an. Hierbei wird der neue Datensatz aufsteigend
   --  sortiert eingefügt.
   --
   --  PARAMETERS:
   --  + L      : Liste an die angehängt werden soll
   --  + Inhalt : Anzuhängender Inhalt
   procedure Sortiert_Einfuegen (L : in out Liste; Inhalt : Inhalts_Typ);

   --  TYPE Elemente_Bearbeitung
   --
   --  Zeiger auf eine Procedure, welche als in out Parameter ein Element
   --  vom Datentyp Inhalts_Typ erhält.
   type Elemente_Bearbeitung is access procedure (I : in out Inhalts_Typ);

   --  PROCEDURE Bearbeite_Elemente
   --
   --  Ruft für jedes Datenelement der Liste die übergebene Prozedur auf.
   --
   --  PARAMETERS:
   --  + L           : Liste die bearbeitet werden soll
   --  + Bearbeitung : Zeiger auf aufzurufende Procedure
   procedure Bearbeite_Elemente (L : in out Liste;
      Bearbeitung : Elemente_Bearbeitung);
      
   --  PROCEDURE Delete_Ende
   --
   --  löscht den letzten Eintrag in der Liste
   procedure Delete_Ende (L : in out Liste);
   
   --  FUNCTION Get_Ende
   --  
   --  Gibt den Inhalt des letzten Elementes in der Liste aus
   function Get_Ende (L : in Liste) return Inhalts_Typ;
   
   --  FUNCTION ELement_Suchen
   --
   --  sucht ein Element aus der Liste
   --  und gibt den Zugehörigen Zeiger zu dem Element aus
   --
   --  falls kein Element Gefunden
   --    => gib "null" aus
   procedure Element_Suchen (L : in out Liste; I : in Inhalts_Typ; 
      Zeiger_Auf_Gefundenes_Element : in out Inhalts_Typ);
   
-------------------------------------------------------------------------------
--  CURSOR FUNKTIONEN
-------------------------------------------------------------------------------
   --  FUNCTION Cursor_Listen_Anfang
   --
   --  Setzt Cursor auf L.Anfang
   function Cursor_Listen_Anfang (L : in Liste)
      return Listen_Cursor;
   
   --  FUNCTION Cursor_Listen_Ende
   --
   --  Setzt Cursor auf L.Ende
   function Cursor_Listen_Ende (L : in Liste)
      return Listen_Cursor;
      
   --  PROCEDURE Gehe_Vorwaerts
   --
   --  setzt Cursor auf Cursor.Nach
   procedure Gehe_Vorwaerts (C : in out Listen_Cursor);
   
   --  Procedure Gehe_Rueckwaerts
   --
   --  setzt Cursor auf Cursor.Nach
   procedure Gehe_Rueckwaerts (C : in out Listen_Cursor);
   
   --  FUNCTION Ist_Cursor_Zulaessig
   --
   --  gibt zurueck, ob Cursor = null
   function Ist_Cursor_Zulaessig (C : in Listen_Cursor) return Boolean;
   
   --  FUNCTION Inhalt_An_Cursor
   --
   --  gibt Inhalt eines Cursers zurueck
   function Inhalt_An_Cursor (C : in Listen_Cursor) return Inhalts_Typ;
-------------------------------------------------------------------------------

   --  PROCEDURE Elemente_Loeschen
   --
   --  loescht alle Eloemente der Liste mit .Inhalt = I
   procedure Elemente_Loeschen (L : in out Liste; I : Inhalts_Typ);
   
   --  Zeiger auf eine Vergleichsfunktion
   type Funktion is access function (A : Inhalts_Typ; B : Inhalts_Typ)
      return Boolean;
      
   --  PROCEDURE Liste_Sortieren
   --
   --  Bubblesort mit der uebergebenen Vergleichsfunktion
   procedure Liste_Sortieren (L : in out Liste; F : Funktion);
   
   --  PROCEDURE Liste_Anhaengen
   --
   --  haengt L2 an L1 hinten an
   procedure Liste_Anhaengen (L1 : in out Liste; L2 : in Liste);

private
   type Element;
   
   type Ref_Element is access Element;

   type Element is record
      Inhalt : Inhalts_Typ;
      Vor : Ref_Element;
      Nach : Ref_Element;
   end record;

   type Liste is record
      Anfang : Ref_Element;
      Ende : Ref_Element;
   end record;
   
   type Listen_Cursor is record
     Cursor : Ref_Element;
   end record;

end Listen_Paket;