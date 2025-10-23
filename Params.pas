unit Params;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Edit, FMX.Controls.Presentation, System.IOUtils,
  FireDAC.Comp.Client, FireDAC.Stan.Param, Data.DB, Winapi.Windows;

type
  TForm8 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Image1: TImage;
    Button1: TButton;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    Edit3: TEdit;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure InitGroupe;
  public
  end;

var
  Form8: TForm8;

implementation

{$R *.fmx}

uses base;

{ ----------------------------------------------------------------------------- }
{ Charger une image dans le TImage }
procedure TForm8.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    try
      Image1.Bitmap.LoadFromFile(OpenDialog1.FileName);
    except
      on E: Exception do
        ShowMessage('Erreur lors du chargement de l’image : ' + E.Message);
    end;
  end;
end;

{ ----------------------------------------------------------------------------- }
{ Sauvegarde ou mise à jour du groupe (un seul enregistrement) }
procedure TForm8.Button2Click(Sender: TObject);
var
  Q: TFDQuery;
  ImgDir, ImgFile, ImgName: string;
begin
  if (Trim(Edit1.Text) = '') or (Trim(Edit2.Text) = '') or (Trim(Edit3.Text) = '') then
  begin
    ShowMessage('Veuillez remplir Nom du Groupe, Nom du Président et l’adresse.');
    Exit;
  end;

  // Vérification du DataModule
  if (DataModule1 = nil) or (DataModule1.FDConnection1 = nil) then
  begin
    ShowMessage('Erreur : La connexion à la base de données n’est pas initialisée.');
    Exit;
  end;

  if not DataModule1.FDConnection1.Connected then
    DataModule1.FDConnection1.Open;

  // Création du dossier pour l’image
  ImgDir := TPath.Combine(ExtractFilePath(ParamStr(0)), 'ImagesGroupe');
  if not TDirectory.Exists(ImgDir) then
    TDirectory.CreateDirectory(ImgDir);

  // Rendre le dossier caché (optionnel)
  SetFileAttributes(PWideChar(ImgDir), FILE_ATTRIBUTE_HIDDEN);

  // Nom du fichier image basé sur le nom du groupe
  ImgName := StringReplace(Edit1.Text, ' ', '_', [rfReplaceAll]) + '.jpg';
  ImgFile := TPath.Combine(ImgDir, ImgName);

  // Sauvegarder l’image si elle existe
  if not Image1.Bitmap.IsEmpty then
  begin
    try
      Image1.Bitmap.SaveToFile(ImgFile);
    except
      on E: Exception do
      begin
        ShowMessage('Erreur lors de la sauvegarde de l’image : ' + E.Message);
        Exit;
      end;
    end;
  end;

  // Enregistrement des données
  Q := TFDQuery.Create(nil);
  try
    Q.Connection := DataModule1.FDConnection1;

    // Vérifier si une entrée existe déjà
    Q.SQL.Text := 'SELECT COUNT(*) AS nb FROM t_groupe';
    Q.Open;

    if Q.FieldByName('nb').AsInteger = 0 then
    begin
      // Aucun enregistrement → INSERT
      Q.Close;
      Q.SQL.Text :=
        'INSERT INTO t_groupe (nomSetting, presidentSetting, adresseSetting, photoSetting) ' +
        'VALUES (:n, :p, :a, :ph)';
      Q.ParamByName('n').AsString := UpperCase(Edit1.Text);
      Q.ParamByName('p').AsString := Edit2.Text;
      Q.ParamByName('a').AsString := Edit3.Text;
      Q.ParamByName('ph').AsString := ImgName;
      Q.ExecSQL;
      ShowMessage('✅ Groupe enregistré avec succès.');
    end
    else
    begin
      // Déjà existant → UPDATE
      Q.Close;
      Q.SQL.Text :=
        'UPDATE t_groupe SET nomSetting = :n, presidentSetting = :p, adresseSetting = :a, photoSetting = :ph WHERE id = 1';
      Q.ParamByName('n').AsString := UpperCase(Edit1.Text);
      Q.ParamByName('p').AsString := Edit2.Text;
      Q.ParamByName('a').AsString := Edit3.Text;
      Q.ParamByName('ph').AsString := ImgName;
      Q.ExecSQL;
      ShowMessage('🔄 Informations mises à jour avec succès.');
    end;

  finally
    Q.Free;
  end;
end;

{ ----------------------------------------------------------------------------- }
{ Chargement des informations au démarrage }
procedure TForm8.InitGroupe;
var
  Q: TFDQuery;
  PhotoPath: string;
begin
  if (DataModule1 = nil) or (DataModule1.FDConnection1 = nil) then
  begin
    ShowMessage('Erreur : Base de données non initialisée.');
    Exit;
  end;

  if not DataModule1.FDConnection1.Connected then
    DataModule1.FDConnection1.Open;

  Q := TFDQuery.Create(nil);
  try
    Q.Connection := DataModule1.FDConnection1;
    Q.SQL.Text := 'SELECT * FROM t_groupe LIMIT 1';
    Q.Open;

    if not Q.IsEmpty then
    begin
      Edit1.Text := Q.FieldByName('nomSetting').AsString;
      Edit2.Text := Q.FieldByName('presidentSetting').AsString;
      Edit3.Text := Q.FieldByName('adresseSetting').AsString;

      PhotoPath := TPath.Combine(ExtractFilePath(ParamStr(0)), 'ImagesGroupe\' + Q.FieldByName('photoSetting').AsString);

      if TFile.Exists(PhotoPath) then
      begin
        try
          Image1.Bitmap.LoadFromFile(PhotoPath);
        except
          on E: Exception do
            ShowMessage('Erreur lors du chargement de la photo : ' + E.Message);
        end;
      end
      else
        Image1.Bitmap.SetSize(0, 0);
    end;
  finally
    Q.Free;
  end;
end;

{ ----------------------------------------------------------------------------- }
procedure TForm8.FormCreate(Sender: TObject);
begin
  Button1.Text := '📸 Importer une photo';
  Button2.Text := '💾 Enregistrer';
end;

{ ----------------------------------------------------------------------------- }
procedure TForm8.FormShow(Sender: TObject);
begin
  Caption := 'GESTION DE GROUPE (Informations du Groupe)';
  Position := TFormPosition.ScreenCenter;
  InitGroupe;
end;

end.

