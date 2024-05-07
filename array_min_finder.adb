with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Indefinite_Vectors;
with Ada.Numerics.Discrete_Random;

procedure Array_Min_Finder is
   package Random_Gen is new Ada.Numerics.Discrete_Random(Integer);
   use Random_Gen;
   G : Generator;

   package Int_Vector is new Ada.Containers.Indefinite_Vectors (Positive, Integer);

   Data_Vector : Int_Vector.Vector;

   protected type Min_Holder is
      entry Update (Value : Integer);
      function Get return Integer;
   private
      Min : Integer := Integer'Last;
   end Min_Holder;

   protected body Min_Holder is
      entry Update (Value : Integer) when True is
      begin
         if Value < Min then
            Min := Value;
         end if;
      end Update;

      function Get return Integer is
      begin
         return Min;
      end Get;
   end Min_Holder;

   Global_Min : Min_Holder;

   task Controller is
      entry Report_Completion;
      entry Set_Thread_Count (Count : Positive);
   end Controller;

   task body Controller is
      Finished_Count : Integer := 0;
      Thread_Count : Positive := 1;
   begin
      accept Set_Thread_Count (Count : Positive) do
         Thread_Count := Count;
      end Set_Thread_Count;

      for I in 1 .. Thread_Count loop
         accept Report_Completion do
            Finished_Count := Finished_Count + 1;
         end Report_Completion;
      end loop;
      Put_Line ("Execution completed, array minimum: " & Integer'Image(Global_Min.Get));
   end Controller;


   task type Calculator (Start_Index, End_Index : Positive; Id : Integer);
   task body Calculator is
      Local_Min : Integer := Integer'Last;
   begin
      for I in Start_Index .. End_Index loop
         Local_Min := Integer'Min(Local_Min, Data_Vector.Element(I));
      end loop;
	  Put_Line("Caluclator " & Id'Image & " finished on segment [" & Start_Index'Image & " - " & End_Index'Image & "] with minimum: " & Local_Min'Image);
      Global_Min.Update(Local_Min);
      Controller.Report_Completion;
   end Calculator;

   type Calculator_Ptr is access Calculator;
   Calculators : array (1 .. 100) of Calculator_Ptr;

   procedure Initialize_Vector (Size : Positive) is
      Min_Pos : Positive;
   begin
      for I in 1 .. Size loop
         Data_Vector.Append (Random(G) mod 1000);
      end loop;
      Min_Pos := Random(G) mod Size + 1;
      Data_Vector.Replace_Element(Min_Pos, -1);
      Put_Line ("Finished generation of random vector of size " & Integer'Image(Size) &
                ", with minimum hidden at position: " & Integer'Image(Min_Pos));
   end Initialize_Vector;

   function Input_With_Default (Message : String; Def_Value: Positive) return Positive is
		Value: Integer;
   begin
		Put_Line(Message);
		 Get(Value);
		 if Value >= 1 then
			return Value;
		 else
			Put_Line("Invalid input. Proceeding with default value");
			return Def_Value;
		 end if;
   end Input_With_Default;

begin
   declare
      Array_Size : Positive := Input_With_Default("Enter the size of the array:  ", 1_000_000);
      Thread_Count : Positive := Input_With_Default("Enter the number of threads:  ", 10);
      Block_Size : Positive := Array_Size / Thread_Count;
      Remainder : Integer := Array_Size mod Thread_Count;
      Start, Fin : Positive := 1;
   begin
	  Put_Line("Starting");
      Initialize_Vector (Array_Size);

      Controller.Set_Thread_Count (Thread_Count);
	  Put_Line("Starting " & Thread_Count'Image & " threads with uniformly distributed segments:");
      for I in 1 .. Thread_Count loop
         if Remainder > 0 then
            Fin := Start + Block_Size;
            Remainder := Remainder - 1;
         else
            Fin := Start + Block_Size - 1;
         end if;
         Calculators (I) := new Calculator (Start, Fin, I);
         Start := Fin + 1;
      end loop;
   end;
end Array_Min_Finder;
