with HAL;

package body St7789 is

   use type HAL.UInt32;
   use type HAL.Bitmap.Bitmap_Color_Mode;

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

   function New_Device (CS_Pin  : HAL.GPIO.Any_GPIO_Point;
                        RST_Pin : HAL.GPIO.Any_GPIO_Point;
                        DC_Pin  : HAL.GPIO.Any_GPIO_Point;
                        SPI     : HAL.SPI.Any_SPI_Port) return Device
   is
   begin
      return Device'(Initialized => False,
                     CS_Pin      => CS_Pin,
                     RST_Pin     => RST_Pin,
                     DC_Pin      => DC_Pin,
                     SPI         => SPI,
                     Layer       => Bitmap'(Dev    => null,
                                            Area   => HAL.Bitmap.Rect'(Position => HAL.Bitmap.Point'(0, 0),
                                                                       Width    => Screen_Width,
                                                                       Height   => Screen_Height),
                                            Source => (0, 0)));
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
         (HAL.SPI.SPI_Data_8b'(0, HAL.UInt8 (Window.Position.X),
                               0, HAL.UInt8 (Window.Position.X
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
         (HAL.SPI.SPI_Data_8b'(0, HAL.UInt8 (Window.Position.Y),
                               0, HAL.UInt8 (Window.Position.Y
                                             + Window.Height - 1)),
          SPI_Status);
      Status := To_Status (SPI_Status);
      if Status /= OK then
         return;
      end if;
      Dev.Write_Cmd (RAMWR, Status);
   end Set_Window;

   procedure Initialize (Dev    : in out Device;
                         Status :    out Device_Status)
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
      Dev.Initialized := Status = OK;
   end Initialize;

   procedure Turn_Off (Dev    :     Device;
                       Status : out Device_Status)
   is
   begin
      Dev.Write_Cmd (DISPOFF, Status);
   end Turn_Off;

   overriding
   function Max_Layers (This : Device) return Positive is (1);

   overriding
   function Supported (This : Device;
                       Mode : HAL.Framebuffer.FB_Color_Mode) return Boolean is
      (Mode = HAL.Bitmap.RGB_565);

   overriding
   procedure Set_Orientation (This        : in out Device;
                              Orientation :        HAL.Framebuffer.Display_Orientation) is null;

   overriding
   procedure Set_Mode (This : in out Device;
                       Mode :        HAL.Framebuffer.Wait_Mode) is null;

   overriding
   function Initialized (This : Device) return Boolean is (This.Initialized);

   overriding
   function Width (This : Device) return Positive is (Screen_Width);

   overriding
   function Height (This : Device) return Positive is (Screen_Height);

   overriding
   function Swapped (This : Device) return Boolean is (False);

   overriding
   procedure Set_Background (This    : Device;
                             R, G, B : HAL.UInt8) is null;

   overriding
   procedure Initialize_Layer (This   : in out Device;
                               Layer  :        Positive;
                               Mode   :        HAL.Framebuffer.FB_Color_Mode := HAL.Bitmap.RGB_565;
                               X      :        Natural := 0;
                               Y      :        Natural := 0;
                               Width  :        Positive := Screen_Width;
                               Height :        Positive := Screen_Height)
   is
   begin
      if Layer /= 1 or Mode /= HAL.Bitmap.RGB_565 then
         return;
      end if;
      This.Layer.Dev := This'Unrestricted_Access;
      This.Layer.Area.Position.X := X;
      This.Layer.Area.Position.Y := Y;
      This.Layer.Area.Height     := Height;
      This.Layer.Area.Width      := Width;
   end Initialize_Layer;

   overriding
   function Initialized (This  : Device;
                         Layer : Positive) return Boolean is
      (Layer = 1 and then This.Layer.Dev /= null);

   overriding
   procedure Update_Layer (This      : in out Device;
                           Layer     :        Positive;
                           Copy_Back :        Boolean := False) is null;

   overriding
   procedure Update_Layers (This : in out Device) is null;

   overriding
   function Color_Mode (This  : Device;
                        Layer : Positive) return HAL.Framebuffer.FB_Color_Mode is (HAL.Bitmap.RGB_565);

   overriding
   function Hidden_Buffer (This  : in out Device;
                           Layer :        Positive) return not null HAL.Bitmap.Any_Bitmap_Buffer is
      (This.Layer'Unrestricted_Access);

   overriding
   function Pixel_Size (This  : Device;
                        Layer : Positive) return Positive is (16);

   overriding
   procedure Set_Source (This   : in out Bitmap;
                         Source :        HAL.UInt32)
   is
   begin
      This.Source (1) := HAL.UInt8 (HAL.Shift_Right (Source, 8));
      This.Source (2) := HAL.UInt8 (Source);
   end Set_Source;

   overriding
   function Source (This : Bitmap) return HAL.UInt32 is
      (HAL.Shift_Left (HAL.UInt32 (This.Source (1)), 8) + HAL.UInt32 (This.Source (2)));

   overriding
   function Width (This : Bitmap) return Natural is (This.Area.Width);

   overriding
   function Height (This : Bitmap) return Natural is (This.Area.Width);

   overriding
   function Swapped (This : Bitmap) return Boolean is (False);

   overriding
   function Color_Mode (This : Bitmap) return HAL.Bitmap.Bitmap_Color_Mode is (This.Dev.Color_Mode (1));

   overriding
   function Mapped_In_RAM (This : Bitmap) return Boolean is (False);

   overriding
   function Memory_Address (This : Bitmap) return System.Address is (System.Null_Address);

   overriding
   procedure Set_Pixel (This : in out Bitmap;
                        Pt   :        HAL.Bitmap.Point)
   is
   begin
      This.Fill_Rect (HAL.Bitmap.Rect'(Position => Pt,
                                       Width    => 1,
                                       Height   => 1));
   end Set_Pixel;

   overriding
   procedure Set_Pixel_Blend (This : in out Bitmap;
                              Pt   :        HAL.Bitmap.Point) renames Set_Pixel;

   overriding
   function Pixel (This : Bitmap;
                   Pt   : HAL.Bitmap.Point) return HAL.UInt32
   is
   begin
      return raise Program_Error with "not supported";
   end Pixel;

   overriding
   procedure Fill (This : in out Bitmap)
   is
   begin
      This.Fill_Rect (This.Area);
   end Fill;

   overriding
   procedure Fill_Rect (This : in out Bitmap;
                        Area :        HAL.Bitmap.Rect)
   is
      Line : HAL.SPI.SPI_Data_8b (1 .. Area.Width * 2);
      SPI_Status : HAL.SPI.SPI_Status;
      Status     : Device_Status;
   begin
      This.Dev.Set_Window (Area, Status);
      if Status /= OK then
         return;
      end if;
      for C in 1 .. Area.Width loop
         Line (C * 2 - 1) := This.Source (1);
         Line (C * 2) := This.Source (2);
      end loop;
      This.Dev.DC_Pin.Set;
      This.Dev.CS_Pin.Clear;
      for L in 1 .. Area.Height loop
         This.Dev.SPI.Transmit (Line, SPI_Status);
         exit when To_Status (SPI_Status) /= OK;
      end loop;
   end Fill_Rect;

   overriding
   function Buffer_Size (This : Bitmap) return Natural is (0);

end St7789;
