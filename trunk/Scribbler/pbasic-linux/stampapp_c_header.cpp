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

int main(int argc, char *argv[])
{
  int i, c, j;
  FILE* pfile;
  void *hso;  
  char *error;  
  

  STDAPI (*GetVersion)(void);
  STDAPI (*CompileIt)(TModuleRec *, char *Src, bool DirectivesOnly, 
		      bool ParseStampDirectives, TSrcTokReference *Ref); 
  
  STDAPI (*doTestRecAlignment)(TModuleRec *);
  
  if (argc != 2)
    {
      fprintf(stderr, "usage: %s pbasic-filename\n", argv[0]);
      exit(-1);
    }


  /* Open shared library - if path not given below, LD_LIBRARY_PATH environment must be set to find lib */
  hso = dlopen("./tokenizer.so",RTLD_LAZY);
  if (!hso)
    {
      perror(dlerror());    
      exit(EXIT_FAILURE);
    }
  
  /* Map Version function in tokenizer.so to GetVersion*/
  GetVersion= (STDAPI(*)(void))dlsym(hso,"Version");
  
  /* Map Compile function in tokenizer.so to CompileIt*/
  CompileIt= (STDAPI(*)(TModuleRec *,char *Src,bool DirectivesOnly,bool 
			ParseStampDirectives,TSrcTokReference *Ref))dlsym(hso,"Compile");
  
  /* Map TestRecAlignment function in tokenizer.so to doTestRecAlignment*/
  doTestRecAlignment= (STDAPI(*)(TModuleRec *))dlsym(hso,"TestRecAlignment");
  
  if ((error=dlerror())!=NULL)
    {
      perror(error);
      dlclose(hso);
      exit(EXIT_FAILURE);
    }
  
  /* Allocate TModuleRec*/
  ModuleRec = (TModuleRec *)malloc(sizeof(TModuleRec));

  /* Display version of tokenizer */
  //printf("PBASIC Tokenizer Library version %1.2f\n\n", (float)(GetVersion())/100);
  
  pfile = fopen(argv[1], "r");

  if (!pfile)
    {
      fprintf(stderr, "No such file %s", argv[1]);
      exit(-1);
    }

  i = 0;
  while ((c = fgetc(pfile)) != EOF && i < MaxSourceSize) 
    {
      Source[i++] = c;
    }
  fclose(pfile);

  Source[i++] = '\0';

  /* Load source code */
  //strcpy(&Source[0],"'{$STAMP BS2}\nDEBUG \"Hello\"");

  printf ("//Size of %s = %d\n", argv[1], strlen(Source));;
  //  printf(Source);
    
  /* Set source size indicator */
  ModuleRec->SourceSize = strlen(Source);

  /* Compile source */
  CompileIt(ModuleRec, Source, false, true, NULL);  
  //printResults(ModuleRec);

  if (ModuleRec->Succeeded)
    {
      printf("//TModuleRec.PacketCount: %d\n", ModuleRec->PacketCount);
      printf("const unsigned char init_scribbler_prog[INIT_PROG_SIZE] = {\n");
      for (i = 0; i < ModuleRec->PacketCount; i++)
	{
	  for (j = 0; j < 18; j++)
	    {
	      if ((i == (ModuleRec->PacketCount - 1)) && (j == 17))
		{
		  printf("0x%.02x}", ModuleRec->PacketBuffer[i*18+j]);
		}
	      else
		{
		  printf("0x%.02x, ", ModuleRec->PacketBuffer[i*18+j]);
		}
	    }
	  printf("\n");
	}
      printf(";");
    }
  else
    {
      printf("Compile failed %s\n", ModuleRec->Error);
    }
  
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


