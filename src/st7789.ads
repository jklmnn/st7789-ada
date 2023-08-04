with HAL.Bitmap;
with HAL.GPIO;
with HAL.SPI;

package St7789 is

   type Device is tagged limited private;

   type Device_Status is (OK, Error, Timeout, Invalid_Area);

   function New_Device (CS_Pin  : HAL.GPIO.Any_GPIO_Point;
                        RST_Pin : HAL.GPIO.Any_GPIO_Point;
                        DC_Pin  : HAL.GPIO.Any_GPIO_Point;
                        SPI     : HAL.SPI.Any_SPI_Port) return Device;

   procedure Initialize (Dev    :     Device;
                         Status : out Device_Status);

   procedure Draw (Dev    :     Device;
                   Area   :     HAL.Bitmap.Rect;
                   Color  :     HAL.Bitmap.Bitmap_Color;
                   Status : out Device_Status);

   procedure Turn_Off (Dev    :     Device;
                       Status : out Device_Status);

private

   type Device is tagged limited record
      CS_Pin  : HAL.GPIO.Any_GPIO_Point;
      RST_Pin : HAL.GPIO.Any_GPIO_Point;
      DC_Pin  : HAL.GPIO.Any_GPIO_Point;
      SPI     : HAL.SPI.Any_SPI_Port;
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

end St7789;
