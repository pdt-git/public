/*
NOTE:
this file was borrowed from http://win32.mvps.org/
Thanks to the author, Felix Kasza, for putting it into the public domain.
--lu
*/




#include <windows.h>
#include <stdio.h>
#pragma hdrstop



// fkill forces a kill -- it will attempt to enable SeDebugPrivilege
// before opening its process handles, allowing it to kill processes
// running under builtin\system (LocalSystem, to the users out there).


int main( int argc, char *argv[] );
void getDebugPriv( void );



#define isBadHandle(h) ( (h) == NULL || (h) == INVALID_HANDLE_VALUE )
#define lenof(x) ( sizeof (x) / sizeof ((x)[0]) )



const int MAXPID = 1024;



int main( int argc, char *argv[] )
{
	int pidCount, i, errors;
	char *p;
	HANDLE hProcess;
	static DWORD pid[MAXPID];

	// parse args, build PID list
	errors = pidCount = 0;

	for ( i = 1; i < argc; i ++ )
	{
		if ( pidCount == lenof( pid ) ) {
			errors ++;
			break;
		}

		pid[pidCount] = strtol( argv[i], &p, 0 );
		if ( p == argv[i] || *p )
			errors ++;
		else
			pidCount ++;
	}

	if ( errors || pidCount == 0 )
	{
		puts( "Usage: fkill pid [...]" );
		puts( "fkill tries to kill the processes specified by the PIDs. If the" );
		puts( "user has debug privileges, fkill is able to kill system processes." );
		puts( "PIDs may be decimal, octal (starts with 0), or hex (starts with 0x)." );
		return MAXPID + 1;
	}

	// try to acquire SeDebugPrivilege
	getDebugPriv();

	errors = 0;
	// for each PID:
	for ( i = 0; i < pidCount; i ++ )
	{
		printf( "pid %lu: ", pid[i] );

		// open process
		hProcess = OpenProcess( PROCESS_TERMINATE, FALSE, pid[i] );
		if ( isBadHandle( hProcess ) )
			printf( "OpenProcess() failed, err = %lu\n", GetLastError() );
		else
		{
			// kill process
			if ( ! TerminateProcess( hProcess, (DWORD) -1 ) )
				printf( "TerminateProcess() failed, err = %lu\n", GetLastError() );
			else
				puts( "killed." );

			// close handle
			CloseHandle( hProcess );
		}
	}

	return 0;
}



void getDebugPriv( void )
{
	HANDLE hToken;
	LUID sedebugnameValue;
	TOKEN_PRIVILEGES tkp;

	if ( ! OpenProcessToken( GetCurrentProcess(),
		TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &hToken ) )
		return;

	if ( !LookupPrivilegeValue( NULL, SE_DEBUG_NAME, &sedebugnameValue ) )
	{
		CloseHandle( hToken );
		return;
	}

	tkp.PrivilegeCount = 1;
	tkp.Privileges[0].Luid = sedebugnameValue;
	tkp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;

	AdjustTokenPrivileges( hToken, FALSE, &tkp, sizeof tkp, NULL, NULL );

	CloseHandle( hToken );
}
