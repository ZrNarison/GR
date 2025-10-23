unit uSplash;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TFormSplash = class(TForm)
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure UpdateProgress(Value: Integer; const Msg: string = '');
  end;

var
  FormSplash: TFormSplash;

implementation

{$R *.fmx}
procedure TFormSplash.FormCreate(Sender: TObject);
begin
  Position := TFormPosition.ScreenCenter;
end;

procedure TFormSplash.UpdateProgress(Value: Integer; const Msg: string);
begin
  ProgressBar1.Value := Value;
  if Msg <> '' then
    Label1.Text := Msg;
  Application.ProcessMessages; // permet de rafraîchir la barre
end;

end.
