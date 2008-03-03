#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
//#include <iostream>
//#include <string>
#include <windows.h>
 
char* getfontspec(tagLOGFONTA font) {
	long height;
	height = font.lfHeight;
	if (height < 1) {
		HDC hdc;
		hdc = GetDC(0);
		height = (long)(-(float)72 / (float)GetDeviceCaps(hdc, LOGPIXELSY) * (float)height);			
	}
	char* bold = "";
	if (font.lfWeight > FW_REGULAR) bold = " bold";
	char* italic = "";
	if (font.lfItalic) italic = " italic";
	char* underline = "";
	if (font.lfUnderline) underline = " underline";
	char* overstrike = "";
	if (font.lfStrikeOut) overstrike = " overstrike";
	char* res;
	printf(res, "%s %d%s%s%s%s", font.lfFaceName, height, bold, italic, underline, overstrike);
	return(res);
}

void winSystemFonts(char ** out) {
	int size;
	NONCLIENTMETRICS metrics;
	metrics.cbSize = sizeof(metrics);
	if (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, sizeof(metrics), &metrics, 0)) {
		char* font;
		font = getfontspec(metrics.lfMessageFont);
		out[0] = R_alloc(sizeof(font), 1);
	   	out[0] = font;
		font = getfontspec(metrics.lfMenuFont);
		out[1] = R_alloc(sizeof(font), 1);
	   	out[1] = font;		 
		font = getfontspec(metrics.lfCaptionFont);
		out[2] = R_alloc(sizeof(font), 1);
	   	out[2] = font;	
		font = getfontspec(metrics.lfSmCaptionFont);
		out[3] = R_alloc(sizeof(font), 1);
	   	out[3] = font;
		font = getfontspec(metrics.lfStatusFont);
		out[4] = R_alloc(sizeof(font), 1);
	   	out[4] = font;
	} else {
		size = sizeof("");
		out = R_alloc(size, 1);	
	}
}
