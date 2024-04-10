with HAL.Bitmap;
with HAL.GPIO;
with HAL.SPI;
with HAL.Framebuffer;
private with System;
private with Soft_Drawing_Bitmap;

package St7789 is

   type Device is limited new HAL.Framebuffer.Frame_Buffer_Display with
      private;

   type Any_Device is access all Device;

   type Device_Status is (OK, Error, Timeout, Invalid_Area);

   Screen_Width  : constant := 240;
   Screen_Height : constant := 320;

   function New_Device (CS_Pin  : HAL.GPIO.Any_GPIO_Point;
                        RST_Pin : HAL.GPIO.Any_GPIO_Point;
                        DC_Pin  : HAL.GPIO.Any_GPIO_Point;
                        SPI     : HAL.SPI.Any_SPI_Port) return Device;

   procedure Initialize (Dev    : in out Device;
                         Status :    out Device_Status);

   procedure Turn_Off (Dev    :     Device;
                       Status : out Device_Status);

   overriding
   function Max_Layers (This : Device) return Positive;

   overriding
   function Supported (This : Device;
                       Mode : HAL.Framebuffer.FB_Color_Mode) return Boolean;

   overriding
   procedure Set_Orientation (This        : in out Device;
                              Orientation :        HAL.Framebuffer.Display_Orientation);

   overriding
   procedure Set_Mode (This : in out Device;
                       Mode :        HAL.Framebuffer.Wait_Mode);

   overriding
   function Initialized (This : Device) return Boolean;

   overriding
   function Width (This : Device) return Positive;

   overriding
   function Height (This : Device) return Positive;

   overriding
   function Swapped (This : Device) return Boolean;

   overriding
   procedure Set_Background (This    : Device;
                             R, G, B : HAL.UInt8);

   overriding
   procedure Initialize_Layer (This   : in out Device;
                               Layer  :        Positive;
                               Mode   :        HAL.Framebuffer.FB_Color_Mode := HAL.Bitmap.RGB_565;
                               X      :        Natural := 0;
                               Y      :        Natural := 0;
                               Width  :        Positive := Screen_Width;
                               Height :        Positive := Screen_Height);

   overriding
   function Initialized (This  : Device;
                         Layer : Positive) return Boolean;

   overriding
   procedure Update_Layer (This      : in out Device;
                           Layer     :        Positive;
                           Copy_Back :        Boolean := False);

   overriding
   procedure Update_Layers (This : in out Device);

   overriding
   function Color_Mode (This  : Device;
                        Layer : Positive) return HAL.Framebuffer.FB_Color_Mode;

   overriding
   function Hidden_Buffer (This  : in out Device;
                           Layer :        Positive) return not null HAL.Bitmap.Any_Bitmap_Buffer;

   overriding
   function Pixel_Size (This  : Device;
                        Layer : Positive) return Positive;

private

   type Bitmap is new Soft_Drawing_Bitmap.Soft_Drawing_Bitmap_Buffer with record
      Dev    : Any_Device;
      Area   : HAL.Bitmap.Rect;
      Source : HAL.UInt16;
   end record;

   type Device is limited new HAL.Framebuffer.Frame_Buffer_Display with record
      Initialized : Boolean;
      CS_Pin      : HAL.GPIO.Any_GPIO_Point;
      RST_Pin     : HAL.GPIO.Any_GPIO_Point;
      DC_Pin      : HAL.GPIO.Any_GPIO_Point;
      SPI         : HAL.SPI.Any_SPI_Port;
      Layer       : aliased Bitmap;
   end record;

   type CTL is (MLRGB) with
      Size => 8;

   for CTL use (MLRGB => 16#10#);

   type Command is (NOP,
                    SWRESET,
                    RDDID,
                    RDDST,
                    SLPIN,
                    SLPOUT,
                    PTLON,
                    NORON,
                    INVOFF,
                    INVON,
                    DISPOFF,
                    DISPON,
                    CASET,
                    RASET,
                    RAMWR,
                    RAMRD,
                    PTLAR,
                    MADCTL,
                    COLMOD,
                    RDID1,
                    RDID2,
                    RDID3,
                    RDID4);

   for Command use (NOP     => 16#00#,
                    SWRESET => 16#01#,
                    RDDID   => 16#04#,
                    RDDST   => 16#09#,
                    SLPIN   => 16#10#,
                    SLPOUT  => 16#11#,
                    PTLON   => 16#12#,
                    NORON   => 16#13#,
                    INVOFF  => 16#20#,
                    INVON   => 16#21#,
                    DISPOFF => 16#28#,
                    DISPON  => 16#29#,
                    CASET   => 16#2A#,
                    RASET   => 16#2B#,
                    RAMWR   => 16#2C#,
                    RAMRD   => 16#2E#,
                    PTLAR   => 16#30#,
                    MADCTL  => 16#36#,
                    COLMOD  => 16#3A#,
                    RDID1   => 16#DA#,
                    RDID2   => 16#DB#,
                    RDID3   => 16#DC#,
                    RDID4   => 16#DD#);

   procedure Write_Cmd (Dev    :     Device;
                        Cmd    :     Command;
                        Status : out Device_Status;
                        Cont   :     Boolean := False);

   procedure Write_Data (Dev    :     Device;
                         Cmd    :     Command;
                         Data   :     HAL.SPI.SPI_Data_8b;
                         Status : out Device_Status);

   procedure Write_Data (Dev    :     Device;
                         Cmd    :     Command;
                         Data   :     HAL.SPI.SPI_Data_16b;
                         Status : out Device_Status);

   procedure Set_Window (Dev    :     Device;
                         Window :     HAL.Bitmap.Rect;
                         Status : out Device_Status);

   overriding
   procedure Set_Source (This   : in out Bitmap;
                         Source :        HAL.UInt32);

   overriding
   function Source (This : Bitmap) return HAL.UInt32;

   overriding
   function Width (This : Bitmap) return Natural;

   overriding
   function Height (This : Bitmap) return Natural;

   overriding
   function Swapped (This : Bitmap) return Boolean;

   overriding
   function Color_Mode (This : Bitmap) return HAL.Bitmap.Bitmap_Color_Mode;

   overriding
   function Mapped_In_RAM (This : Bitmap) return Boolean;

   overriding
   function Memory_Address (This : Bitmap) return System.Address;

   overriding
   procedure Set_Pixel (This : in out Bitmap;
                        Pt   :        HAL.Bitmap.Point);

   overriding
   procedure Set_Pixel_Blend (This : in out Bitmap;
                              Pt   :        HAL.Bitmap.Point);

   overriding
   function Pixel (This : Bitmap;
                   Pt   : HAL.Bitmap.Point) return HAL.UInt32;

   overriding
   procedure Fill (This : in out Bitmap);

   overriding
   procedure Fill_Rect (This : in out Bitmap;
                        Area :        HAL.Bitmap.Rect);

   overriding
   function Buffer_Size (This : Bitmap) return Natural;

end St7789;
