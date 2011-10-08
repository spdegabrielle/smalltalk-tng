digraph OSDependencies {
	rankdir=LR;
	size="7.5,10";

	bootasm;
	driverframework -> langdesign;
	driverframework -> bootasm;
	idedriver -> driverframework;
	kbdriver -> driverframework;
	mousedriver -> driverframework;
	ethdriver -> driverframework;
	tcpip -> ethdriver;
	tcpip -> bitsyntax;
	vesamodeswitch -> langdesign;
	fontdata -> langdesign;
	bitblt;
	cairolike;
	objmem;
	gc -> objmem;
	gui -> cairolike;
	gui -> bitblt;
	gui -> fontdata
	gui -> fontmetrics;
	langdesign -> patternmatcher;
	langdesign -> bitsyntax;
	langdesign -> weakpointerhierarchy;
	patternmatcher;
	bitsyntax;
	compiler -> langdesign;
	interpreter -> langdesign;
	imageloader -> objmem
	imageloader -> langdesign;
	imageformat -> objmem;
	imagesaver -> objmem
	imagesaver -> langdesign;
	gitlike -> sha1;
	gitlike -> blockstorage;
	blockstorage -> idedriver;
	sha1;
	weakpointerhierarchy
	fontmetrics;
	paragraphlayout -> gui;
	paragrapheditor -> paragraphlayout;
	listwidget -> gui;
	buttonwidget -> gui;
	become -> objmem;
	debugger -> objmem;
	debugger -> interpreter;
	emergencyevaluator -> objmem;
	emergencyevaluator -> langdesign;
	fat32 -> idedriver;
	acpi -> driverframework;
	smtpclient -> tcpip;
	httpclient -> tcpip;
	imapclient -> tcpip;
	assembler -> bitsyntax;
	disassembler -> bitsyntax;
	dynamiclinker -> objmem;
	dependencymanager -> objmem;
	tableeditor -> gui;
}