with HAL;
with Interfaces;

package body St7789 is

   function To_Status (S : HAL.SPI.SPI_Status) return Device_Status
   is
   begin
      case S is
         when HAL.SPI.Ok =>
            return OK;
         when HAL.SPI.Err_Error =>
            return Error;
         when HAL.SPI.Err_Timeout | HAL.SPI.Busy =>
            return Timeout;
      end case;
   end To_Status;

   procedure Hard_Reset (Dev : Device)
   is
   begin
      Dev.CS_Pin.Clear;
      Dev.RST_Pin.Set;
      Dev.RST_Pin.Clear;
      Dev.RST_Pin.Set;
      Dev.CS_Pin.Set;
   end Hard_Reset;

   procedure Write_Cmd (Dev    :     Device;
                        Cmd    :     Command;
                        Status : out Device_Status;
                        Cont   :     Boolean := False)
   is
      SPI_Data : constant HAL.SPI.SPI_Data_8b :=
         (0 => HAL.UInt8'(Cmd'Enum_Rep));
      SPI_Status : HAL.SPI.SPI_Status;
   begin
      Dev.CS_Pin.Clear;
      Dev.DC_Pin.Clear;
      Dev.SPI.Transmit (SPI_Data, SPI_Status);
      Status := To_Status (SPI_Status);
      if not Cont then
         Dev.CS_Pin.Set;
      end if;
   end Write_Cmd;

   procedure Write_Data (Dev    :     Device;
                         Cmd    :     Command;
                         Data   :     HAL.SPI.SPI_Data_8b;
                         Status : out Device_Status)
   is
      SPI_Status : HAL.SPI.SPI_Status;
   begin
      Dev.Write_Cmd (Cmd, Status, True);
      if Status /= OK then
         return;
      end if;
      Dev.DC_Pin.Set;
      Dev.SPI.Transmit (Data, SPI_Status);
      Status := To_Status (SPI_Status);
      Dev.CS_Pin.Clear;
   end Write_Data;

   procedure Write_Data (Dev    :     Device;
                         Cmd    :     Command;
                         Data   :     HAL.SPI.SPI_Data_16b;
                         Status : out Device_Status)
   is
      SPI_Status : HAL.SPI.SPI_Status;
   begin
      Dev.Write_Cmd (Cmd, Status, True);
      if Status /= OK then
         return;
      end if;
      Dev.DC_Pin.Set;
      Dev.SPI.Transmit (Data, SPI_Status);
      Status := To_Status (SPI_Status);
      Dev.CS_Pin.Clear;
   end Write_Data;

   function New_Device (CS_Pin  : HAL.GPIO.Any_GPIO_Point;
                        RST_Pin : HAL.GPIO.Any_GPIO_Point;
                        DC_Pin  : HAL.GPIO.Any_GPIO_Point;
                        SPI     : HAL.SPI.Any_SPI_Port) return Device
   is
   begin
      return Device'(CS_Pin  => CS_Pin,
                     RST_Pin => RST_Pin,
                     DC_Pin  => DC_Pin,
                     SPI     => SPI);
   end New_Device;

   procedure Set_Window (Dev    :     Device;
                         Window :     HAL.Bitmap.Rect;
                         Status : out Device_Status)
   is
      SPI_Status : HAL.SPI.SPI_Status;
   begin
      if
         Window.Position.X > Natural (HAL.UInt8'Last)
         or else Window.Position.Y > Natural (HAL.UInt8'Last)
         or else Window.Position.X + Window.Width - 1
                  > Natural (HAL.UInt8'Last)
         or else Window.Position.Y + Window.Height - 1
                  > Natural (HAL.UInt8'Last)
      then
         Status := Invalid_Area;
         return;
      end if;
      Dev.Write_Cmd (CASET, Status, True);
      if Status /= OK then
         return;
      end if;
      Dev.DC_Pin.Set;
      Dev.SPI.Transmit
         (HAL.SPI.SPI_Data_8b'(HAL.UInt8 (Window.Position.X),
                               HAL.UInt8 (Window.Position.X
                                          + Window.Width - 1)),
          SPI_Status);
      Status := To_Status (SPI_Status);
      if Status /= OK then
         return;
      end if;
      Dev.Write_Cmd (RASET, Status, True);
      if Status /= OK then
         return;
      end if;
      Dev.DC_Pin.Set;
      Dev.SPI.Transmit
         (HAL.SPI.SPI_Data_8b'(HAL.UInt8 (Window.Position.Y),
                               HAL.UInt8 (Window.Position.Y
                                          + Window.Width - 1)),
          SPI_Status);
      Status := To_Status (SPI_Status);
      if Status /= OK then
         return;
      end if;
      Dev.Write_Cmd (RAMWR, Status);
   end Set_Window;

   procedure Initialize (Dev    :     Device;
                         Status : out Device_Status)
   is
   begin
      Dev.RST_Pin.Set_Mode (HAL.GPIO.Output);
      Dev.CS_Pin.Set_Mode (HAL.GPIO.Output);
      Dev.DC_Pin.Set_Mode (HAL.GPIO.Output);
      Dev.Hard_Reset;
      Dev.Write_Cmd (SLPOUT, Status);
      if Status /= OK then
         return;
      end if;
      --  RGB565
      Dev.Write_Data (COLMOD, HAL.SPI.SPI_Data_8b'(0 => 16#55#), Status);
      if Status /= OK then
         return;
      end if;
      Dev.Write_Data (MADCTL, HAL.SPI.SPI_Data_8b'(0 => 16#10#), Status);
      if Status /= OK then
         return;
      end if;
      Dev.Set_Window (HAL.Bitmap.Rect'(Position =>
                                          HAL.Bitmap.Point'(X => 0, Y => 0),
                                       Width    => 240,
                                       Height   => 240),
                      Status);
      if Status /= OK then
         return;
      end if;
      Dev.Write_Cmd (INVON, Status);
      if Status /= OK then
         return;
      end if;
      Dev.Write_Cmd (NORON, Status);
      if Status /= OK then
         return;
      end if;
      Dev.Write_Cmd (DISPON, Status);
   end Initialize;

   procedure Turn_Off (Dev    :     Device;
                       Status : out Device_Status)
   is
   begin
      Dev.Write_Cmd (DISPOFF, Status);
   end Turn_Off;

   function RGB888_To_RGB565 (Color : HAL.Bitmap.Bitmap_Color)
      return HAL.UInt16
   is
      use Interfaces;
      Red, Green, Blue : Unsigned_16 := 0;
   begin
      Blue  := Shift_Right (Unsigned_16 (Color.Blue), 3) and 16#1F#;
      Green :=
         Shift_Left (Shift_Right (Unsigned_16 (Color.Green), 2) and 16#3F#, 5);
      Red   :=
         Shift_Left (Shift_Right (Unsigned_16 (Color.Red), 3) and 16#1F#, 11);
      return HAL.UInt16 (Red or Green or Blue);
   end RGB888_To_RGB565;

   procedure Draw (Dev    :     Device;
                   Area   :     HAL.Bitmap.Rect;
                   Color  :     HAL.Bitmap.Bitmap_Color;
                   Status : out Device_Status)
   is
      Line : constant HAL.SPI.SPI_Data_16b (0 .. Area.Width - 1) :=
         (others => RGB888_To_RGB565 (Color));
      SPI_Status : HAL.SPI.SPI_Status;
   begin
      Dev.Set_Window (Area, Status);
      if Status /= OK then
         return;
      end if;
      Dev.DC_Pin.Set;
      Dev.CS_Pin.Clear;
      for L in Area.Position.Y .. Area.Height - 1 loop
         Dev.SPI.Transmit (Line, SPI_Status);
         exit when To_Status (SPI_Status) /= OK;
      end loop;
   end Draw;

end St7789;
