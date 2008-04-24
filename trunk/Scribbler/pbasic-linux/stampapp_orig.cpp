/* This is an example of interfacing to the PBASIC Tokenizer Shared Library for the Linux operating system.
   This program links in the tokenizer, displays results from TestRecAlignment routine, then sends a sample
   PBASIC source Code to the tokenizer and reports the results */
   
/* Define includes */
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include "tokenizer.h"

/* Define global variables */
TModuleRec 	*ModuleRec;
char		Source[MaxSourceSize];      /*Load source code here*/

/* Forward Declarations */
void printSize(TModuleRec *Rec);
void printResults(TModuleRec *Rec);

/*-----------------------------------------------------------------------*/

int main(void)
{
  /*---------- Variable declarations -----------*/ 
  int  Idx;

  /*--------- Shared-library overhead ----------*/
  void *hso;             															   				/* handle to dynamic shared library */ 
  char *error; 																	 					/* variable returned from dlerror() */
 
  STDAPI (*GetVersion)(void);													  	 				/* function prototype for Version */
  STDAPI (*CompileIt)(TModuleRec *, char *Src, bool DirectivesOnly, bool ParseStampDirectives, TSrcTokReference *Ref); 	/* function prototype for Compile */
  STDAPI (*doTestRecAlignment)(TModuleRec *); 								/* function prototype for TestRecAlignment */

  /* Open shared library - if path not given below, LD_LIBRARY_PATH environment must be set to find lib */
  hso = dlopen("./tokenizer.so",RTLD_LAZY);
  if (!hso){perror(dlerror());exit(EXIT_FAILURE);}

  /* Map Version function in tokenizer.so to GetVersion*/
  GetVersion= (STDAPI(*)(void))dlsym(hso,"Version");
  /* Map Compile function in tokenizer.so to CompileIt*/
  CompileIt= (STDAPI(*)(TModuleRec *,char *Src,bool DirectivesOnly,bool ParseStampDirectives,TSrcTokReference *Ref))dlsym(hso,"Compile");
  /* Map TestRecAlignment function in tokenizer.so to doTestRecAlignment*/
  doTestRecAlignment= (STDAPI(*)(TModuleRec *))dlsym(hso,"TestRecAlignment");
  
  if ((error=dlerror())!=NULL){perror(error);dlclose(hso);exit(EXIT_FAILURE);}
  /*------ End of Shared-library overhead -------*/

  /* Allocate TModuleRec*/
  ModuleRec = (TModuleRec *)malloc(sizeof(TModuleRec));

  /* Display version of tokenizer */
  printf("PBASIC Tokenizer Library version %1.2f\n\n", (float)(GetVersion())/100);
 
  /*----------------- Test ModuleRec ------------------*/
  /* Display the size of all of the TModuleRec fields. */
  printf("-----------------------------------------\n");
  printf("       SIZE OF TMODULEREC ELEMENTS\n");
  printf("-----------------------------------------\n");
  printSize(ModuleRec);
  /* Call TestRecAlignment and display the results of the TModuleRec fields. */
  doTestRecAlignment(ModuleRec);
  printf("-----------------------------------------\n");
  printf("       RESULTS OF TESTRECALIGNMENT\n");
  printf("-----------------------------------------\n");
  printResults(ModuleRec);
  
  
  /*------------------ Compile Code ------------------*/  
  /* Load source code */
  strcpy(&Source[0],"'{$STAMP BS2}\nDEBUG \"Hello\"");
  /* Set source size indicator */
  ModuleRec->SourceSize = strlen(&Source[0]);
  printf(&Source[0]);
  /* Compile source */
  CompileIt(ModuleRec,&Source[0],false,true,NULL);  
  printf("\n-----------------------------------------\n");
  printf("   RESULTS OF SOURCE CODE COMPILATION\n");
  printf("-----------------------------------------\n");
  printResults(ModuleRec);
  
  
  dlclose(hso);     /* close shared library */
  return 0;
}

/*-----------------------------------------------------------------------*/

void printSize(TModuleRec *Rec)
{
  printf("TModuleRec.Succeeded: %d\n", sizeof(Rec->Succeeded));
  printf("TModuleRec.Error: %d\n", sizeof(Rec->Error));
  printf("TModuleRec.DebugFlag: %d\n", sizeof(Rec->DebugFlag));
  printf("TModuleRec.TargetModule: %d\n", sizeof(Rec->TargetModule));
  printf("TModuleRec.TargetStart: %d\n", sizeof(Rec->TargetStart));
  printf("TModuleRec.ProjectFiles: %d\n", sizeof(Rec->ProjectFiles));
  printf("TModuleRec.ProjectFilesStart: %d\n", sizeof(Rec->ProjectFilesStart));
  printf("TModuleRec.Port: %d\n", sizeof(Rec->Port));
  printf("TModuleRec.PortStart: %d\n", sizeof(Rec->PortStart));
  printf("TModuleRec.LanguageVersion: %d\n", sizeof(Rec->LanguageVersion));
  printf("TModuleRec.LanguageStart: %d\n", sizeof(Rec->LanguageStart));
  printf("TModuleRec.SourceSize: %d\n", sizeof(Rec->SourceSize));
  printf("TModuleRec.ErrorStart: %d\n", sizeof(Rec->ErrorStart));
  printf("TModuleRec.ErrorLength: %d\n", sizeof(Rec->ErrorLength));
  printf("TModuleRec.EEPROM: %d\n", sizeof(Rec->EEPROM));
  printf("TModuleRec.EEPROMFlags: %d\n", sizeof(Rec->EEPROMFlags));
  printf("TModuleRec.VarCounts: %d\n", sizeof(Rec->VarCounts));
  printf("TModuleRec.PacketCount: %d\n", sizeof(Rec->PacketCount));
  printf("TModuleRec.PacketBuffer: %d\n", sizeof(Rec->PacketBuffer));
}

/*-----------------------------------------------------------------------*/

void printResults(TModuleRec *Rec)
{
  int Idx;

  printf("TModuleRec.Succeeded: %d\n", Rec->Succeeded);
  printf("TModuleRec.Error: %s\n", Rec->Error);
  printf("TModuleRec.DebugFlag: %d\n", Rec->DebugFlag);
  printf("TModuleRec.TargetModule: %d\n", Rec->TargetModule);
  printf("TModuleRec.TargetStart: %d\n", Rec->TargetStart);
  for (Idx = 0; Idx < 7; Idx++)
    printf("TModuleRec.ProjectFiles[%d]: %s\n", Idx, Rec->ProjectFiles[Idx]);
  for (Idx = 0; Idx < 7; Idx++)
    printf("TModuleRec.ProjectFilesStart[%d]: %d\n", Idx,Rec->ProjectFilesStart[Idx]);
  printf("TModuleRec.Port: %s\n", Rec->Port);
  printf("TModuleRec.PortStart: %d\n", Rec->PortStart);
  printf("TModuleRec.LanguageVersion: %d\n", Rec->LanguageVersion);
  printf("TModuleRec.LanguageStart: %d\n", Rec->LanguageStart);
  printf("TModuleRec.SourceSize: %d\n", Rec->SourceSize);
  printf("TModuleRec.ErrorStart: %d\n", Rec->ErrorStart);
  printf("TModuleRec.ErrorLength: %d\n", Rec->ErrorLength);
  for (Idx = 0; Idx < EEPROMSize; Idx++)
    printf("TModuleRec.EEPROM[%d]: %d\n", Idx, Rec->EEPROM[Idx]);
  for (Idx = 0; Idx < EEPROMSize; Idx++)
    printf("TModuleRec.EEPROMFlags[%d]: %d\n", Idx, Rec->EEPROMFlags[Idx]);
  for (Idx = 0; Idx < 4; Idx++)
    printf("TModuleRec.VarCounts[%d]: %d\n", Idx, Rec->VarCounts[Idx]);
  printf("TModuleRec.PacketCount: %d\n", Rec->PacketCount);
  for (Idx = 0; Idx < sizeof(TPacketType); Idx++)
    printf("TModuleRec.PacketBuffer[%d]: %d\n", Idx, Rec->PacketBuffer[Idx]);
}


