/*  
    Arduino teatimer

    ******* CAREFUL ***********************************
     the calculations below expect Vcc = 5V, not 3.3V
    ***************************************************
    check  http://www.arduino.cc/playground/ComponentLib/Thermistor2

http://blog.makezine.com/archive/2011/10/how-to-shrinkify-your-arduino-projects.html?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+makezineonline+%28MAKE%29
http://electronics.stackexchange.com/questions/4531/serial-newbie-why-cant-i-just-hook-the-wires-up
http://arduino.cc/forum/index.php?topic=58756.0
http://home.ict.nl/~fredkrom/pe0fko/g8voi_guide/
http://www.avrfreaks.net/index.php?name=PNphpBB2&file=index
http://sensorapp.net/?p=479
http://wiring.org.co/learning/basics/airqualitymq135.html

dust sensor DISABLED!

*/

#include <math.h>
#include <DHT.h>
#include <Time.h>
#define DHTTYPE DHT22   // DHT 22  (AM2302)
///////////////////////////////////////////////////////
//CONFIG

const int teatimer = 60 * 3;   //set timer to x min

//PINS settings
//analog
const int gasSensor = 5; // select input pin for gasSensor
const int dustPin=3;
const int thermistorPin = 0;
const int LDRpin = 2;
//digital
const int DHTPIN = 7;
//output
const int backLight = 13;      // pin 13 will control the backlight


const int clogstamp = 60 * 5; //wait x minutes between each collection of temp/humidity
//start time
time_t pctime = 1331020700;

///////////////////////////////////////////////////////

// Setup a DHT22 instance
DHT dht(DHTPIN, DHTTYPE);

static boolean brewing = false;
static boolean pickitup = false;
time_t timer;
time_t logstamp;

int gasval = 0; // variable to store the value coming from the gas sensor

//settings for the IR LED dust sensor
int dustVal=0;
int LDRVal = 0;
int ledPower=2;
int delayTime=280;
int delayTime2=40;
float offTime=9680;

double temp2 = 0;


void setup(void) {
  Serial.begin(9600);
  pinMode(backLight, OUTPUT);
  pinMode(ledPower,OUTPUT);
  pinMode(4, OUTPUT);
  
  digitalWrite(backLight, LOW); // turn backlight on. Replace 'HIGH' with 'LOW' to turn it off.
  temp2 = Thermister(analogRead(thermistorPin));
  setTime(pctime);
  Serial.println("time,temperature,humidity,tea,dust,gas,light");
  logstamp = now() + clogstamp;

}



void loop(void) {

 //wait 5 min
 //delay(300000 - 5000);
  
//  dustVal=analogRead(dustPin); // read the dust value via pin 5 on the sensor
  float h = dht.readHumidity();
  float t = dht.readTemperature();
  digitalWrite(ledPower,LOW); // power on the LED
  delayMicroseconds(delayTime);
//  dustVal=analogRead(dustPin); // read the dust value via pin 5 on the sensor
  dustVal = 0;
  delayMicroseconds(delayTime2);
  digitalWrite(ledPower,HIGH); // turn the LED off
  delayMicroseconds(offTime);

  //gasval = analogRead(gasSensor); // read the value from the pot
  gasval = analogRead(gasSensor);
  delay(2000);
  gasval = analogRead(gasSensor); // read the value from the pot
  delay(1000);
  LDRVal = analogRead(LDRpin);
  delay(2000);
  LDRVal = analogRead(LDRpin);
  if (isnan(t) || isnan(h)) {
    Serial.println("Failed to read from DHT");
  } else {
    if(now() > logstamp) {
      logstamp = now() + clogstamp;
      Serial.print(now());
      Serial.print(",");    
      Serial.print(t);
      Serial.print(",");
      Serial.print(h);
      Serial.print(",");
      int b = brewing?1:0;
      Serial.print(b);
      Serial.print(",");
      Serial.print(dustVal);
      Serial.print(",");
      Serial.print(gasval);
      Serial.print(",");
      Serial.println(LDRVal);
    }
  }
  
  double temp = Thermister(analogRead(thermistorPin));
  //if increase of more than 0.8C in 10 sec (loop delay)
  // -> must be a hot drink near the sensor
  if( (temp - temp2 > 0.8) & (brewing == false) ) {
    //Serial.println("Brewing...");
    digitalWrite(backLight, HIGH);
    brewing = true;
    timer = now();
    //trigger logging
    logstamp = now();
  }
  
  if( (now() > timer + teatimer) & brewing ) {
    //Serial.println("Tea is ready!");
    digitalWrite(backLight, LOW);
    brewing = false;
    pickitup = true;
  }
  
  if(pickitup == true) {
      int blink = LOW;
      int counter = 0;
      //blink until surface cools, meaning drink was picked up or cold now
      while((temp - temp2 >= -0.2) && counter < 100) {
           counter++;
           blink = !blink;
           digitalWrite(backLight, blink);

           delay(500);
           //only poll temperature every 5 seconds
           //to give enough time to get a correct gradient
           if( counter % 10 == 0 ) {
               temp2 = temp;
               temp = Thermister(analogRead(thermistorPin));
           }

      }
      pickitup = false;
      digitalWrite(backLight, LOW);
  }
  
  //printTemp();
  temp2 = temp;  
}

double Thermister(int RawADC) {
  double Temp;
  // See http://en.wikipedia.org/wiki/Thermistor for explanation of formula
  Temp = log(((10240000/RawADC) - 10000));
  Temp = 1 / (0.001129148 + (0.000234125 * Temp) + (0.0000000876741 * Temp * Temp * Temp));
  Temp = Temp - 273.15;           // Convert Kelvin to Celsius
  return Temp;
}

void printTemp(void) {
  double fTemp;
  double temp = Thermister(analogRead(thermistorPin));  // Read sensor
  Serial.println(temp);
}
