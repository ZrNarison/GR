unit reup;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit,
  System.Character, FMX.DialogService;

type
  TForm9 = class(TForm)
    Edit1: TEdit; // CIN
    Edit2: TEdit; // Mot de passe
    Edit4: TEdit; // Confirmation mot de passe
    Button2: TButton; // Valider
    Button3: TButton; // Annuler
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    procedure Button2Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form9: TForm9;
  Tentatives: Integer = 0;

implementation

{$R *.fmx}

uses base, login, Reunion;

procedure TForm9.Edit1Change(Sender: TObject);
var
  CIN: string;
begin
  CIN := Trim(Edit1.Text);

  if Length(CIN) = 12 then
  begin
    if CharInSet(CIN[6], ['1','2']) then
    begin
      Edit1.TextSettings.FontColor := TAlphaColorRec.Green;
      Edit1.Hint := 'CIN valide';
    end
    else
    begin
      Edit1.TextSettings.FontColor := TAlphaColorRec.Red;
      TDialogService.ShowMessage('Le 6ème chiffre du CIN doit être 1 ou 2.');
      Edit1.SetFocus;
    end;
  end
  else if Length(CIN) > 12 then
  begin
    Edit1.TextSettings.FontColor := TAlphaColorRec.Red;
    TDialogService.ShowMessage('Le numéro CIN doit contenir exactement 12 chiffres.');
    Edit1.Text := Copy(CIN, 1, 12);
    Edit1.SelStart := Length(Edit1.Text);
  end;
end;

procedure TForm9.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Form5.Show;
  Form9.close;
end;

procedure TForm9.FormCreate(Sender: TObject);
begin
    Position := TFormPosition.ScreenCenter;
end;

procedure TForm9.FormShow(Sender: TObject);
begin
    Position := TFormPosition.ScreenCenter;
    CheckBox2.Text:='';
end;

procedure TForm9.Button2Click(Sender: TObject);
begin
  // Vérification des champs obligatoires
  if (Trim(Edit1.Text) = '') or (Trim(Edit2.Text) = '') or (Trim(Edit4.Text) = '') then
  begin
    TDialogService.ShowMessage('Veuillez remplir tous les champs.');
    Exit;
  end;

  // Vérification si CIN existe dans t_membre
  with DataModule1.FDQuery3 do
  begin
    Close;
    SQL.Text := 'SELECT cin FROM t_membre WHERE cin = :cin';
    ParamByName('cin').AsString := Edit1.Text;
    Open;

    if Eof then
    begin
      TDialogService.ShowMessage('Pseudo d''identification non trouvé.');
      Inc(Tentatives);
      if Tentatives >= 5 then
      begin
        TDialogService.ShowMessage('Trop de tentatives échouées. L''application va se fermer. Merci de contacter votre Président.');
        Application.Terminate;
      end;
      Exit;
    end;
    Close;
  end;

  // Vérifier mot de passe et confirmation
  if Edit2.Text = Edit4.Text then
  begin
    with DataModule1.FDQuery3 do
    begin
      Close;
      SQL.Text := 'SELECT cin FROM t_utilisateur WHERE cin = :cin';
      ParamByName('cin').AsString := Edit1.Text;
      Open;

      if not Eof then
      begin
        // Mise à jour du mot de passe
        Close;
        SQL.Text := 'UPDATE t_utilisateur SET mdp = :mdp WHERE cin = :cin';
        ParamByName('mdp').AsString := Edit2.Text;
        ParamByName('cin').AsString := Edit1.Text;
        ExecSQL;

        TDialogService.ShowMessage('Mot de passe mis à jour avec succès !');
        Form1.Show;
        Close;
      end
      else
      begin
        // CIN non trouvé dans t_utilisateur → ne pas créer
        TDialogService.ShowMessage('Cet utilisateur n''existe pas encore dans la base. Contactez l''administrateur.');
        Inc(Tentatives);
        if Tentatives >= 5 then
        begin
          TDialogService.ShowMessage('Trop de tentatives échouées. L''application va se fermer. Merci de contacter votre Président.');
          Application.Terminate;
        end;
        Exit;
      end;
    end;
  end
  else
  begin
    // Mauvaise confirmation
    TDialogService.ShowMessage('Code de confirmation incorrect, merci de réessayer !');
    Edit2.Text := '';
    Edit4.Text := '';
    Edit2.SetFocus;

    Inc(Tentatives);
    if Tentatives >= 5 then
    begin
      TDialogService.ShowMessage('Trop de tentatives échouées. L''application va se fermer. Merci de contacter votre Président.');
      Application.Terminate;
    end;
  end;
end;

end.

