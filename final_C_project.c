#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h> 
#include <math.h>

/*PURPOSE
What-This C Program enables a robot to find a wall, follow a wall and
escape through the door. In order to achieve this, a communication
protocol in a sense has to be established between computer and robot
through a USB. In order to achieve this, files are written and read into
the USB. In addition, a text file is created from the C program and data
is visualized by reading and writing into this created file. Then, the data 
is parsed and the distances are stored in point cloud to order to allow the robot to make key decisions like calculating the
distance of a door and avoid hitting a wall straight on. Some
limitations of the robot are: Assuming the classroom is empty when the
robot tries to escape and the only escape route is moving against a wall. The
robot is also limited to movement only and the robot does not have a capacity to deal with obstacles.*/

//PROBLEM APPROACH

// struct that stores scan measuremnent 
struct Scan 
{
	int dist; 
	int intensity; 
	int err; 
};
// array that stores data
struct Scan a[360]; 


char line[100];

// USB communication to robot
char  ROBOTUSB[] = "/dev/cu.usbmodem146241";
FILE *fptr; // usb file
FILE *scans_file; // file for lidar scans



// parses scan from a LDS text-file, gets distance and puts in array
void parseScan() 
{

	const char sep[] = ",";
	int angle=0;
	

	fclose(scans_file);
	scans_file = fopen("wall_measurement.txt", "r");


	while(fgets(line, sizeof(line), scans_file) != NULL) 
	{ 
		
		if(isdigit(line[0]))// remove character strings 
		{
			char *parse = strtok(line, sep);
			parse = strtok(NULL, sep);
			
			int i = 0;
			while(parse != NULL) 
			{
				if(i == 0) 
				{
					a[angle].dist = atoi(parse);
				}
				else if(i == 1)
			    {
					a[angle].intensity = atoi(parse);
				}
				else if(i == 2) 
				{
					a[angle].err = atoi(parse);
				}
				i++;
				parse = strtok(NULL, sep);
			}
			angle++;
		}   
	} 
}  


// takes the LDS scan from the USB file and transfers it into a LDS text-file
void getScan() 
{
    
    fclose(scans_file); //we close the text-file because we open it in the main
	scans_file = fopen("wall_measurement.txt", "w");
    char getldsscan[]="getldsscan";
    fprintf(fptr,"%s\n",getldsscan);
    fgets(line, sizeof(line), fptr);
    char* rotation="ROTATION_SPEED";
    
	char line[100];
     int count=0;
      fgets(line, sizeof(line), fptr);

     fgets(line, sizeof(line), fptr);
     
     while(strncmp(line, rotation, 14)!=0)  //stop until rotation speed line
     {  
          printf("count=%d  \t", count);
          fprintf(scans_file,"%s",line);            
          printf("%s", line);
          count++;
          fgets(line, sizeof(line), fptr);
     }
    printf("scan complete\n");
    
}  
// returns the distance at a given angle
int getDistanceAtAngle(int angle) 
{

	return a[angle].dist;
}

// returns the intensity at a given angle
int getIntensityAtAngle(int angle) 
{
	
	return a[angle].intensity;
}

// returns the error at a given angle
int getErrorAtAngle(int angle) 
{
	
	return a[angle].err;
}

// turns the robot on
void turnRobotOn() 
{

	char testmodeon[]="testmode on";
	fprintf(fptr,"%s\n", testmodeon);
	fgets(line, sizeof(line), fptr);
	printf("%s\n", line);

	
}

// turns the robot off
void turnRobotOff() 
{
	
	char testmodeoff[]="testmode off";
	fprintf(fptr,"%s\n", testmodeoff);
	fgets(line, sizeof(line), fptr);
	printf("%s\n", line);

}

// turns the lidar on
void turnLidarOn() 
{
     
     char set_lds_rotation_on[]="setldsrotation on";
	fprintf(fptr,"%s\n",set_lds_rotation_on);
	fgets(line, sizeof(line), fptr);
	printf("%s\n", line);
	
}

// turns the lidar off
void turnLidarOff() 
{
   
   char set_lds_rotation_off[]="setldsrotation off";
   fprintf(fptr,"%s\n",set_lds_rotation_off);
   fgets(line, sizeof(line), fptr);
   printf("%s\n",  line);
}

// convert int to string
char* itoa(int val, int base)
{
	static char buf[32] = {0};
	int i = 30;
	
	for(; val && i ; --i, val /= base) 
	{
		buf[i] = "0123456789abcdef"[val % base];
	}
	
	return &buf[i+1];
}

//function to move robot
void setMotor(int leftWheelDist, int rightWheelDist, int speed, double time_duration) //doesnt work with input zero
{

	
	char src[50], dest[50];
	strcpy(src,  "setmotor ");
	strcpy(dest,  itoa(leftWheelDist, 10));

	
	char* str1 = strcat(src, dest) ;
	
	char* str2 = strcat(str1, " ");
	
	char* str3 = strcat(str2, itoa(rightWheelDist, 10));
	
	char* str4 = strcat(str3, " ");
	
	char* driveforward = strcat(str4, itoa(speed, 10));

	fprintf(fptr,"%s\n",driveforward);

	sleep(time_duration);

	
}

//desired door angles  
struct angle3{
    int i;
    int f;
    int m;
};


//find infinity angles (nothing detected) and filter out distances greater than 2.5 m
int result [100]={};

int * infinity_angles (int l[]){
    int i,o;
    o=0;
    for( i = 0; i < 180; i++)
    {
        if (l[i]==0 && l[i+1]==0 )
        {
            result [o]=i;
            o++;
            
            
            for ( i=i+1; i < 180; i++)
            {
                if (l[i]!=0 && l[i+1]!=0 && l[i]<2500)
                {
                    
                    result[o]=i-1;
                    o++;
                    
                    break;
                }
                
            }
        }
        
    }
    
    printf ("\n");
    return result;
};



//finding biggest continuous gap  
struct angle3  biggest_gap(int l2[]){
    int max=0;
    int initial;
    int final;
    

    for (int i=1;i<20; i=i+2)
        
    {   int dif=l2[i]-l2[i-1];
        if (dif>max)
        {
            max=dif;
            initial=l2[i-1];
            final=l2[i];
        };
    }

    printf ("max=%d\n",max);
    printf ("initial=%d\n",initial);
    printf ("final=%d\n",final);
    struct angle3 result;
    result.i = initial;
    result.f = final;
    result.m = max;
    
    return result;
}



//cosine rule to find desired distance 
double door_distance(double initial, double final, double angle_diff)
{
      
	double final_squared=final*final;
	double initial_squared=initial*initial;
	double ftimesi=initial*final;

	double max_angle= cos(angle_diff*(M_PI/180));

	double door_dist=sqrt((initial_squared+final_squared)-(2 * (ftimesi))* max_angle);

	return door_dist;



}
//sound library
char playsound[]="playsound 0";
char playsound2[]="playsound 1";
char playsound3[]="playsound 2";
char playsound4[]="playsound 3";

int main()
{
	fptr = fopen(ROBOTUSB, "w+");
	scans_file = fopen("wall_measurement.txt", "w+");
	
	 

	turnRobotOn();
	turnLidarOn();
	sleep(1);
	
	//Booting phase
	while(getDistanceAtAngle(0)==0)
	{
		getScan();
		sleep(1);
		parseScan();
		sleep(1);
	}


	//Finding wall
	//At angle 0 we measured the front distance 
	int wall_distance=800;
	
	//Movement after hitting the wall	
	getScan();
	sleep(1);
	parseScan();
	sleep(1);
	
	// in the while loop, there are two if statements: 
	// the first one is used to keep the robot stay in the room
	// the second one is used to get the robot out of the room, if the door is big enough
	while (1)
	{
		
		setMotor(200,200,100,0.05);
		sleep(1);
		getScan();
		sleep(1);
		parseScan();
		sleep(1);
		
		fprintf(fptr,"%s\n", playsound3);
    	fprintf(scans_file,"%s\n", playsound3);
    	
    	  	
		if((getDistanceAtAngle(0)<800) && (getDistanceAtAngle(0)!=0))
		{
			//The robot is close to the wall

			fprintf(fptr,"%s\n", playsound);
    		fprintf(scans_file,"%s\n", playsound);
			
			setMotor(450,5,200,0.05); //
			sleep(2);
			getScan();
			sleep(2);
			parseScan();
			sleep(2);
		}
			


		if((getErrorAtAngle(90)==8035) && (getDistanceAtAngle(150)!=0)) //Find the gap at angle 90
								//8035 is errorcodeHex
		{
			//The gap is evaluated and decides if the robot can get through
				fprintf(fptr,"%s\n", playsound2);
    			fprintf(scans_file,"%s\n", playsound2);	
				sleep(4);
				int distance[180];
				struct angle3 r; //r is the angle when the robot is 
							//trying to identify if the gap is a door
   				r=biggest_gap(infinity_angles (distance));
   				printf ("i=%d\n", r.i);
   				printf ("f=%d\n", r.f);
   				printf ("m=%d\n", r.m);
   				printf("distance at initial angle-------%d\n",getDistanceAtAngle(r.i-1));
   				printf("distance at final angle-------%d\n",getDistanceAtAngle(r.f+1));
   				sleep(2);
 				double door_calculation=door_distance(getDistanceAtAngle(r.i-1),getDistanceAtAngle(r.f+1),r.m);
 				printf ("------door distance%f\n------", door_calculation);
 				sleep(2);
 				int robotsize=370;


				//create an array with distances from all angles 
				for (int i = 0; i < 180; i++)
				{	

	        	distance[i]=getDistanceAtAngle(i);

	        

	       		 }

	       		 if (door_calculation>robotsize) //As long as door is wider than robot
				{
					fprintf(fptr,"%s\n", playsound4);
    				fprintf(scans_file,"%s\n", playsound4);
    				setMotor(-100 ,-100,100,0.05); //
					sleep(2);
					setMotor(5 ,450,200,0.05); //
					sleep(2);
					setMotor(200,200,200,0.05); 
					sleep(1);
					setMotor(200,200,200,0.05); 
					sleep(1);
					setMotor(200,200,200,0.05); 
					sleep(1);
					setMotor(200,200,200,0.05); 
					sleep(1);
					setMotor(200,200,200,0.05); 
					sleep(1);
					break;
					//The loop ends here 
					
					
				}

				
				
			}

		}
		
	turnLidarOff();
	sleep(1);
	turnRobotOff();
	
	

	fclose(fptr); 
	fclose(scans_file);
	
		

}
	
		

	

	


	
	
	
	


