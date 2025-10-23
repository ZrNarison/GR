program GR;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Dialogs,
  System.SysUtils,
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF }
  Reunion in 'Reunion.pas' {Form1},
  base in 'base.pas' {DataModule1: TDataModule},
  Membre in 'Membre.pas' {Form2},
  Fiv in 'Fiv.pas' {Form3},
  user in 'user.pas' {Form4},
  login in 'login.pas' {Form5},
  Rfiv in 'Rfiv.pas' {Form6},
  uSplash in 'uSplash.pas' {FormSplash},
  Social in 'Social.pas' {Form7},
  Params in 'Params.pas' {Form8},
  reup in 'reup.pas' {Form9},
  Depense in 'Depense.pas' {Form10};

{$R *.res}
var
  MutexHandle: THandle;
begin
  {$IFDEF MSWINDOWS}  // seulement pour Windows
    // --- V�rifie si l'application est d�j� lanc�e ---
  MutexHandle := CreateMutex(nil, True, 'GR_UniqueMutex_Name');
  if (MutexHandle = 0) or (GetLastError = ERROR_ALREADY_EXISTS) then
  begin
    ShowMessage('L''application est d�j� ouverte.');
    Exit; // quitte le programme
  end;
    {$ENDIF}


  Application.Initialize;
  
  FormSplash := TFormSplash.Create(nil);
  FormSplash.Show;
  FormSplash.UpdateProgress(10, 'Chargement Form1...');
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm5, Form5);
  Application.CreateForm(TForm9, Form9);
  Application.CreateForm(TForm10, Form10);
  FormSplash.UpdateProgress(30, 'Chargement Form2...');
  Application.CreateForm(TForm2, Form2);

  FormSplash.UpdateProgress(50, 'Chargement Form3...');
  Application.CreateForm(TForm3, Form3);

  FormSplash.UpdateProgress(70, 'Chargement Form4...');
  Application.CreateForm(TForm4, Form4);

//  FormSplash.UpdateProgress(90, 'Chargement Form5...');
//  Application.CreateForm(TForm5, Form5);

  FormSplash.UpdateProgress(100, 'Chargement termin�.');
  Sleep(300); // petite pause
  FormSplash.Close;
  FormSplash.Free;

  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm5, Form5);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TForm4, Form4);
  Application.CreateForm(TForm6, Form6);
  Application.CreateForm(TForm8, Form8);
  Application.CreateForm(TFormSplash, FormSplash);
  Application.Run;
end.
