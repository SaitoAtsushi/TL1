
#include <windows.h>
#include <ctype.h>
#include <stdio.h>
#include "runtime.h"

static HANDLE hBuffer;
static HANDLE hStdOutput;

BOOL WINAPI controlHandler(DWORD type) {
  tl1_io_finalize();
  return FALSE;
}

void tl1_io_init(void) {
  hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
  hBuffer = CreateConsoleScreenBuffer(GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, CONSOLE_TEXTMODE_BUFFER, NULL);
  if(!SetConsoleActiveScreenBuffer(hBuffer)) {
    fprintf(stderr, "Fail to set screen buffer.");
    CloseHandle(hBuffer);
    exit(1);
  }
  SetConsoleCtrlHandler(controlHandler, TRUE);
}

void tl1_io_finalize(void) {
  SetConsoleActiveScreenBuffer(hStdOutput);
  CloseHandle(hBuffer);
}

void tl1_putch(unsigned char ch) {
  void cls(HANDLE hConsole);
  if(ch==12) cls(hBuffer);
  else if(ch=='\n') {
    CONSOLE_SCREEN_BUFFER_INFO info;
    GetConsoleScreenBufferInfo(hBuffer, &info);
    COORD coordCursor = info.dwCursorPosition;
    coordCursor.Y++;
    coordCursor.X=0;
    SetConsoleCursorPosition(hBuffer, coordCursor);
  } else {
    DWORD written;
    WriteConsoleA(hBuffer, &ch, 1, &written, 0);
  }
}

void tl1_putstr(char* str) {
  DWORD written;
  WriteConsoleA(hBuffer, str, strlen(str), &written, 0);
}

#include <stdio.h>

void tl1_putnum(unsigned char x) {
  char buf[4];
  DWORD written;
  wsprintfA(buf, "%d", x);
  WriteConsoleA(hBuffer, buf, strlen(buf), &written, 0);
}

void tl1_puthex(unsigned char x) {
  char buf[4];
  DWORD written;
  wsprintfA(buf, "%02x", x);
  WriteConsoleA(hBuffer, buf, strlen(buf), &written, 0);
}

void tl1_putright(unsigned char c, unsigned char x) {
  char buf[c+1];
  DWORD written;
  wsprintfA(buf, "%*d", c, x);
  WriteConsoleA(hBuffer, buf, strlen(buf), &written, 0);
}

unsigned char f_get_sys(unsigned char dev) {
  INPUT_RECORD inputRecord;
  DWORD dwReadEvent;

  HANDLE hStdInput = GetStdHandle(STD_INPUT_HANDLE);

  while(1) {
    if (!ReadConsoleInput(hStdInput, &inputRecord, 1, &dwReadEvent))
      continue;
    if (!(inputRecord.EventType == KEY_EVENT
          && inputRecord.Event.KeyEvent.bKeyDown))
      continue;

    UINT key = MapVirtualKey(inputRecord.Event.KeyEvent.wVirtualKeyCode, 2);
    if(key) return inputRecord.Event.KeyEvent.uChar.AsciiChar;
    else continue;
  }
  return 0;
}

unsigned char f_read_sys(unsigned char dev) {
  TCHAR szBuf[4];
  char buffer[4];
  INPUT_RECORD inputRecord;
  DWORD dwWriteByte, dwReadEvent;

  HANDLE hStdInput = GetStdHandle(STD_INPUT_HANDLE);

  CONSOLE_SCREEN_BUFFER_INFO info;
  
  GetConsoleScreenBufferInfo(hBuffer, &info);
  COORD coordCursor = info.dwCursorPosition;

  for(int i=0; ReadConsoleInput(hStdInput, &inputRecord, 1, &dwReadEvent);) {
    if (!(inputRecord.EventType == KEY_EVENT &&
          inputRecord.Event.KeyEvent.bKeyDown))
      continue;
    switch(inputRecord.Event.KeyEvent.wVirtualKeyCode) {
    case VK_RETURN :
      if(i>0) return strtol(buffer, NULL, 10);
    case VK_BACK :
      if (--coordCursor.X < 0) coordCursor.X = 0;
      else {
        SetConsoleCursorPosition(hBuffer, coordCursor);
        WriteConsoleOutputCharacterA(hBuffer, " ", 1, coordCursor, &dwWriteByte);
        buffer[--i]='\0';
      }
      break;
    default :
      {
        UINT key = MapVirtualKey(inputRecord.Event.KeyEvent.wVirtualKeyCode, 2);
        if (isdigit(key)) {
          buffer[i++]=key; buffer[i]='\0';
          if(strtol(buffer, NULL, 10)<256) {
            szBuf[0]=key; szBuf[1]='\0';
            WriteConsoleOutputCharacterA(hBuffer, szBuf, 1, coordCursor, &dwWriteByte);
            coordCursor.X++;
            SetConsoleCursorPosition(hBuffer, coordCursor);
          } else {
            buffer[--i]='\0';
          }
        }
      }
    }
  }
  return 0;
}

void cls(HANDLE hConsole ) {
  COORD coordScreen = { 0, 0 };
  DWORD cCharsWritten;
  CONSOLE_SCREEN_BUFFER_INFO csbi;
  DWORD dwConSize;

  GetConsoleScreenBufferInfo( hConsole, &csbi );
  dwConSize = csbi.dwSize.X * csbi.dwSize.Y;
  FillConsoleOutputCharacter( hConsole, (TCHAR) ' ',
                                         dwConSize, coordScreen, &cCharsWritten );
  GetConsoleScreenBufferInfo( hConsole, &csbi );
  FillConsoleOutputAttribute( hConsole, csbi.wAttributes,
                                         dwConSize, coordScreen, &cCharsWritten );
  SetConsoleCursorPosition( hConsole, coordScreen );
}
