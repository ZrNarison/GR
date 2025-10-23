unit Reunion;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Layouts, FMX.Ani,
  FMXTee.Engine, FMXTee.Procs, FMXTee.Chart, FMXTee.Series,
  System.IOUtils, System.JSON, Data.DB, FireDAC.Comp.Client, System.DateUtils,
  System.NetEncoding, System.Zip, base, FMX.ListBox, FMX.DialogService;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Timer1: TTimer;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    ProgressBar1: TProgressBar;
    Chart1: TChart;
    Rectangle1, Rectangle2, Rectangle3, Rectangle4, Rectangle5, Rectangle6: TRectangle;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Rectangle2Click(Sender: TObject);
    procedure Rectangle3Click(Sender: TObject);
    procedure Rectangle4Click(Sender: TObject);
    procedure Rectangle6Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    procedure ChargerImage(Image: TImage; const NomFichier: string);
    procedure InitialiserBDD;
    procedure Security;
    procedure CalculerStats;
    procedure AjouterDonnees(DateSel: TDate; NbReunion, NbSocial, NbAbsents, TotalMembre: Integer);
    procedure ShowSplash;
    procedure ImporterTables;
    procedure ExporterTables;
    procedure RemplirComboDates;
    procedure InitialiserChart;
    var
      SerieReunion, SerieSocial, SerieAbsents: TLineSeries;
      ConfigPath, LogPath,CheminDB: string;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{$R *.LgXhdpiTb.fmx ANDROID}
{$R *.SmXhdpiPh.fmx ANDROID}
{$R *.LgXhdpiPh.fmx ANDROID}
{$R *.Windows.fmx MSWINDOWS}

uses
  Membre, Fiv, user, Rfiv, uSplash, Social, Params,
  Winapi.Windows, Depense; // pour Mutex sur Windows

// -------------------------------------------------------------------
procedure TForm1.Security;
var
  hMutex: THandle;
begin
  {$IFDEF MSWINDOWS}
  hMutex := CreateMutex(nil, True, 'GR_APP_INSTANCE');
  if (hMutex = 0) or (GetLastError = ERROR_ALREADY_EXISTS) then
  begin
    MessageBox(0, 'L''application est déjà en cours d''exécution !', 'Attention', MB_ICONWARNING);
    Halt;
  end;

  ConfigPath := TPath.Combine(TPath.GetHomePath, 'AppData\Roaming\MonApp\Config');
  LogPath := TPath.Combine(TPath.GetHomePath, 'AppData\Roaming\MonApp\Logs');
  CheminDB := TPath.Combine(TPath.GetHomePath, 'AppData\Roaming\MonApp\data\bdd.db');

  {$ENDIF}
end;

// -------------------------------------------------------------------
procedure TForm1.ShowSplash;
var
  Splash: TFormSplash;
  i: Integer;
begin
  Splash := TFormSplash.Create(nil);
  try
    Splash.Show;
    for i := 1 to 10 do
    begin
      Splash.UpdateProgress(i, Format('Chargement %d%%...', [i]));
      Application.ProcessMessages;
    end;
  finally
    Splash.Free;
  end;
end;

// -------------------------------------------------------------------
procedure TForm1.InitialiserChart;
begin
  Chart1.SeriesList.Clear;

  SerieReunion := TLineSeries.Create(Self);
  SerieReunion.Title := 'Présents Réunion';
  SerieReunion.SeriesColor := TAlphaColorRec.Blue;
  SerieReunion.Marks.Visible := True;
  SerieReunion.Pointer.Visible := True;
  Chart1.AddSeries(SerieReunion);

  SerieSocial := TLineSeries.Create(Self);
  SerieSocial.Title := 'Activités Sociales';
  SerieSocial.SeriesColor := TAlphaColorRec.Green;
  SerieSocial.Marks.Visible := True;
  SerieSocial.Pointer.Visible := True;
  Chart1.AddSeries(SerieSocial);

  SerieAbsents := TLineSeries.Create(Self);
  SerieAbsents.Title := 'Absents';
  SerieAbsents.SeriesColor := TAlphaColorRec.Red;
  SerieAbsents.Marks.Visible := True;
  SerieAbsents.Pointer.Visible := True;
  Chart1.AddSeries(SerieAbsents);

  Chart1.LeftAxis.Title.Text := 'Pourcentage (%)';
  Chart1.View3D := False;
end;


// -------------------------------------------------------------------
procedure TForm1.AjouterDonnees(DateSel: TDate; NbReunion, NbSocial, NbAbsents, TotalMembre: Integer);
begin
  if Chart1.SeriesCount < 3 then Exit;

  Chart1.Series[0].AddXY(DateSel, (NbReunion / TotalMembre) * 100);
  Chart1.Series[1].AddXY(DateSel, (NbSocial / TotalMembre) * 100);
  Chart1.Series[2].AddXY(DateSel, (NbAbsents / TotalMembre) * 100);
end;

// -------------------------------------------------------------------
procedure TForm1.CalculerStats;
var
  Q, QDate: TFDQuery;
  TotalMembre, NbReunion, NbSocial, NbAbsents: Integer;
  DateSel: TDate;
begin
  if Chart1.SeriesCount < 3 then
    InitialiserChart;

  Chart1.Series[0].Clear;
  Chart1.Series[1].Clear;
  Chart1.Series[2].Clear;

  Q := TFDQuery.Create(nil);
  QDate := TFDQuery.Create(nil);
  try
    Q.Connection := DataModule1.FDConnection1;
    QDate.Connection := DataModule1.FDConnection1;

    Q.SQL.Text := 'SELECT COUNT(*) AS total FROM t_membre';
    Q.Open;
    TotalMembre := Q.FieldByName('total').AsInteger;
    Q.Close;

    if TotalMembre = 0 then
    begin
      ShowMessage('Aucun membre trouvé dans la base.');
      Exit;
    end;

    QDate.SQL.Text := 'SELECT DISTINCT date_reunion FROM t_reunion ORDER BY date_reunion';
    QDate.Open;

    while not QDate.EOF do
    begin
      DateSel := StrToDate(QDate.FieldByName('date_reunion').AsString);

      Q.SQL.Text := 'SELECT COUNT(DISTINCT idMembre) AS total FROM t_presence ' +
                    'WHERE idReunion IN (SELECT id FROM t_reunion WHERE date_reunion = :date)';
      Q.ParamByName('date').AsDate := DateSel;
      Q.Open;
      NbReunion := Q.FieldByName('total').AsInteger;
      Q.Close;

      Q.SQL.Text := 'SELECT COUNT(DISTINCT idMembre) AS total FROM t_social ' +
                    'WHERE idReunion IN (SELECT id FROM t_reunion WHERE date_reunion = :date)';
      Q.ParamByName('date').AsDate := DateSel;
      Q.Open;
      NbSocial := Q.FieldByName('total').AsInteger;
      Q.Close;

      Q.SQL.Text := 'SELECT COUNT(*) AS total FROM t_membre ' +
                    'WHERE id NOT IN (SELECT DISTINCT idMembre FROM t_presence ' +
                    'WHERE idReunion IN (SELECT id FROM t_reunion WHERE date_reunion = :date))';
      Q.ParamByName('date').AsDate := DateSel;
      Q.Open;
      NbAbsents := Q.FieldByName('total').AsInteger;
      Q.Close;

      AjouterDonnees(DateSel, NbReunion, NbSocial, NbAbsents, TotalMembre);

      QDate.Next;
    end;

  finally
    Q.Free;
    QDate.Free;
  end;
end;

// -------------------------------------------------------------------
procedure TForm1.InitialiserBDD;
var
  CheminDB,DossierData: string;
begin
  if not Assigned(DataModule1) then
    raise Exception.Create('DataModule1 non initialisé !');
//       --------PREPARATION DE LA BASE DE DONNEES-----------------/
//    Chémin de la BDD dont il créer s'il n'éxiste pas
    {$IFDEF MSWINDOWS}
  // Sous Windows : dans le dossier la même que le Path
    CheminDB := TPath.Combine(ExtractFilePath(ParamStr(0)), 'bdd.db');
    {$ENDIF}

    {$IFDEF MACOS}
    // Sous macOS : même chose (Documents)
    CheminDB := TPath.Combine(TPath.GetDocumentsPath, 'bdd.db');
    {$ENDIF}

    {$IFDEF ANDROID}
    // Sous Android : dans le dossier local de l'application
    CheminDB := TPath.Combine(TPath.GetDocumentsPath, 'bdd.db');
    {$ENDIF}

    // Vérifier et créer le dossier si nécessaire
 DossierData := TPath.Combine(ExtractFilePath(ParamStr(0)), 'data');
 if not TDirectory.Exists(DossierData) then
     TDirectory.CreateDirectory(DossierData);

      // Rendre le dossier caché
  SetFileAttributes(PWideChar(DossierData), FILE_ATTRIBUTE_HIDDEN);

  CheminDB := TPath.Combine(DossierData, 'bdd.db');

//    Supprimer la BDD s'il existe
//    if TFile.Exists(CheminDB) then
//    TFile.Delete(CheminDB);
    with DataModule1.FDConnection1 do
        begin
          DriverName := 'SQLite';
          Params.Database := CheminDB;
          LoginPrompt := False;
          Connected := True;   // Connexion ouverte avant ExecSQL
        end;
  with DataModule1.FDQuery1 do
  begin
    Connection := DataModule1.FDConnection1;
//    Table des membres
//    ExecSQL('DROP TABLE IF EXISTS t_membre');
    ExecSQL(
      'CREATE TABLE IF NOT EXISTS t_membre (' +
      'id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      'matricule INTEGER ,' +
      'nom TEXT NOT NULL,' +
      'prenom TEXT NOT NULL,' +
      'date_naissance TEXT,' +
      'lieu_naissance TEXT,' +
      'filiation_pere TEXT,' +
      'filiation_mere TEXT,' +
      'sexe TEXT,' +
      'cin TEXT UNIQUE,' +
      'date_delivrance TEXT,' +
      'adresse TEXT,' +
      'email TEXT,' +
      'telephone TEXT,' +
      'image TEXT,' +
      'date_inscription TEXT)'
    );
//    ExecSQL('DROP TABLE IF EXISTS t_reunion');
    ExecSQL(
      'CREATE TABLE IF NOT EXISTS t_reunion (' +
      'id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      'date_reunion TEXT)'
    );

//    Table presence
//    ExecSQL('DROP TABLE IF EXISTS t_presence');  //Effacer le table t_presence  s'il existe
    ExecSQL(
      'CREATE TABLE IF NOT EXISTS t_presence (' +  //Créer s'il n'existe pas
      'id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      'idMembre INTEGER,' +
      'idReunion INTEGER,' +
      'remarque TEXT,' +
      'valeur FLOAT,' +
      'FOREIGN KEY(idMembre) REFERENCES t_membre(id),' +
      'FOREIGN KEY(idReunion) REFERENCES t_reunion(id))'
    );
    //    Table social
//    ExecSQL('DROP TABLE IF EXISTS t_social');  //Effacer le table t_presence  s'il existe
    ExecSQL(
      'CREATE TABLE IF NOT EXISTS t_social (' +  //Créer s'il n'existe pas
      'id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      'idMembre INTEGER,' +
      'idReunion INTEGER,' +
      'remarque TEXT,' +
      'valeur FLOAT,' +
      'FOREIGN KEY(idMembre) REFERENCES t_membre(id),' +
      'FOREIGN KEY(idReunion) REFERENCES t_reunion(id))'
    );
//    Table Depense
    ExecSQL(
      'CREATE TABLE IF NOT EXISTS t_depense (' +
      'id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      'date_depense DATE,' +
      'motif TEXT NOT NULL,' +
      'montant REAL NOT NULL)'
    );

//    Table des punitions
    ExecSQL(
      'CREATE TABLE IF NOT EXISTS t_punition (' +
      'id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      'matricule INTEGER,' +
      'date_punition TEXT,' +
      'motif TEXT,' +
      'montant REAL DEFAULT 0,' +
      'FOREIGN KEY(matricule) REFERENCES t_membre(id))'
    );

    // Table des prêts
    ExecSQL(
      'CREATE TABLE IF NOT EXISTS t_pret (' +
      'id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      'matricule INTEGER,' +
      'date_pret TEXT,' +
      'somme REAL NOT NULL,' +
      'interet REAL DEFAULT 0,' +
      'FOREIGN KEY(matricule) REFERENCES t_membre(id))'
    );

    // Table des utilisateurs
    ExecSQL(
      'CREATE TABLE IF NOT EXISTS t_utilisateur (' +
      'id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      'cin TEXT UNIQUE,' +
      'mdp TEXT UNIQUE,' +
      'categorie TEXT NOT NULL)'
    );

    // Table des Setting
//    ExecSQL('DROP TABLE IF EXISTS t_groupe');
    ExecSQL(
      'CREATE TABLE IF NOT EXISTS t_groupe (' +
      'id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      'nomSetting TEXT,' +
      'presidentSetting TEXT,' +
      'adresseSetting TEXT,' +
      'photoSetting TEXT)'
    );
  end;
  with DataModule1.FDConnection1 do
    begin
      Connected := True;  // <-- connexion ouverte après ExecSQL ! ❌
    end;

end;

// -------------------------------------------------------------------
procedure TForm1.ChargerImage(Image: TImage; const NomFichier: string);
var
  Chemin: string;
begin
  Chemin := TPath.Combine(TPath.GetDocumentsPath, NomFichier);
  if TFile.Exists(Chemin) then
    Image.Bitmap.LoadFromFile(Chemin)
  else
    Image.Bitmap.SetSize(1,1);
end;

// -------------------------------------------------------------------
procedure TForm1.RemplirComboDates;
var
  Q: TFDQuery;
begin
//  ComboBox1.Clear;
//  Q := TFDQuery.Create(nil);
//  try
//    Q.Connection := DataModule1.FDConnection1;
//    Q.SQL.Text := 'SELECT DISTINCT date_reunion FROM t_reunion ORDER BY date_reunion';
//    Q.Open;
//    while not Q.EOF do
//    begin
//      ComboBox1.Items.Add(DateToStr(Q.FieldByName('date_reunion').AsDateTime));
//      Q.Next;
//    end;
//  finally
//    Q.Free;
//  end;
end;

// -------------------------------------------------------------------
procedure TForm1.ExporterTables;
var
  SaveDlg: TSaveDialog;
  FileName, TempFile, ZipFile: string;
  Tables: array[0..6] of string;
  i, j, RecordCount, TotalCount: Integer;
  Query: TFDQuery;
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
  Field: TField;
  JSONStr, Chiffre: string;
  Zip: TZipFile;
begin
  SaveDlg := TSaveDialog.Create(nil);
  try
    SaveDlg.Filter := 'Fichier export sécurisé (*.zip)|*.zip';
    SaveDlg.FileName := Format('GRExport_%s.zip', [FormatDateTime('yyyymmdd_hhnnss', Now)]);

    if not SaveDlg.Execute then Exit;

    FileName := SaveDlg.FileName;
    TempFile := TPath.ChangeExtension(FileName, '.dat');
    ZipFile := FileName;

    Tables[0] := 't_membre';
    Tables[1] := 't_reunion';
    Tables[2] := 't_presence';
    Tables[3] := 't_social';
    Tables[4] := 't_depense';
    Tables[5] := 't_punition';
    Tables[6] := 't_pret';

    Query := TFDQuery.Create(nil);
    JSONArray := TJSONArray.Create;
    try
      Query.Connection := DataModule1.FDConnection1;
      TotalCount := 0;

      if Assigned(ProgressBar1) then
      begin
        ProgressBar1.Visible := True;
        ProgressBar1.Min := 0;
        ProgressBar1.Max := Length(Tables);
        ProgressBar1.Value := 0;
      end;

      for i := 0 to High(Tables) do
      begin
        Query.SQL.Text := 'SELECT * FROM ' + Tables[i];
        Query.Open;
        RecordCount := 0;

        while not Query.Eof do
        begin
          JSONObject := TJSONObject.Create;
          JSONObject.AddPair('TableName', Tables[i]);

          for j := 0 to Query.Fields.Count - 1 do
          begin
            Field := Query.Fields[j];
            JSONObject.AddPair(Field.FieldName, VarToStr(Field.Value));
          end;

          JSONArray.AddElement(JSONObject);
          Query.Next;
          Inc(RecordCount);
        end;

        Inc(TotalCount, RecordCount);
        Query.Close;

        if Assigned(ProgressBar1) then
        begin
          ProgressBar1.Value := i + 1;
          Application.ProcessMessages;
        end;
      end;

      JSONStr := JSONArray.ToString;
      Chiffre := TNetEncoding.Base64.Encode(JSONStr);
      TFile.WriteAllText(TempFile, Chiffre, TEncoding.UTF8);

      if not TFile.Exists(TempFile) then
        raise Exception.Create('Le fichier temporaire n’a pas été créé.');

      Zip := TZipFile.Create;
      try
        Zip.Open(ZipFile, zmWrite);
        Zip.Add(TempFile, TPath.GetFileName(TempFile));
        Zip.Close;
      finally
        Zip.Free;
      end;

      TFile.Delete(TempFile);

      if Assigned(ProgressBar1) then
        ProgressBar1.Visible := False;

//      ShowMessage(Format('Exportation réussie (%d enregistrements) vers : %s',
//        [TotalCount, ZipFile]));

    finally
      Query.Free;
      JSONArray.Free;
    end;
  finally
    SaveDlg.Free;
  end;
end;

procedure TForm1.ImporterTables;
var
  OpenDlg: TOpenDialog;
  FileName, TempFile, JSONStr, Dechiffre: string;
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
  i, j: Integer;
  TableName: string;
  Query: TFDQuery;
  FieldsList, ParamsList: string;
  Pair: TJSONPair;
  Zip: TZipFile;
begin
  OpenDlg := TOpenDialog.Create(nil);
  try
    OpenDlg.Filter := 'Fichier export sécurisé (*.zip)|*.zip';
    if not OpenDlg.Execute then Exit;

    FileName := OpenDlg.FileName;
    TempFile := TPath.ChangeExtension(FileName, '.dat');

    // --- Extraction du fichier .dat contenu dans le ZIP ---
    Zip := TZipFile.Create;
    try
      Zip.Open(FileName, zmRead);
      Zip.ExtractAll(TPath.GetDirectoryName(FileName));
      Zip.Close;
    finally
      Zip.Free;
    end;

    if not TFile.Exists(TempFile) then
      raise Exception.Create('Erreur : Le fichier exporté n’a pas été trouvé ou est corrompu.');

    // --- Lecture et décodage ---
    JSONStr := TFile.ReadAllText(TempFile, TEncoding.UTF8);
    Dechiffre := TNetEncoding.Base64.Decode(JSONStr);

    JSONArray := TJSONObject.ParseJSONValue(Dechiffre) as TJSONArray;
    if JSONArray = nil then
      raise Exception.Create('Erreur : le contenu du fichier est invalide.');

    Query := TFDQuery.Create(nil);
    try
      Query.Connection := DataModule1.FDConnection1;

      if Assigned(ProgressBar1) then
      begin
        ProgressBar1.Visible := True;
        ProgressBar1.Min := 0;
        ProgressBar1.Max := JSONArray.Count;
        ProgressBar1.Value := 0;
      end;

      // --- Désactivation des contraintes pour éviter erreurs de clé étrangère ---
      Query.ExecSQL('PRAGMA foreign_keys = OFF;');

      // --- Boucle d’insertion ---
      for i := 0 to JSONArray.Count - 1 do
      begin
        JSONObject := JSONArray.Items[i] as TJSONObject;
        if not JSONObject.TryGetValue<string>('TableName', TableName) then
          Continue;

        FieldsList := '';
        ParamsList := '';
        for j := 0 to JSONObject.Count - 1 do
        begin
          Pair := JSONObject.Pairs[j];
          if Pair.JsonString.Value = 'TableName' then Continue;

          if FieldsList <> '' then
          begin
            FieldsList := FieldsList + ', ';
            ParamsList := ParamsList + ', ';
          end;

          FieldsList := FieldsList + Pair.JsonString.Value;
          ParamsList := ParamsList + ':' + Pair.JsonString.Value;
        end;

        Query.SQL.Text := Format('INSERT OR IGNORE INTO %s (%s) VALUES (%s)',
          [TableName, FieldsList, ParamsList]);

        for j := 0 to JSONObject.Count - 1 do
        begin
          Pair := JSONObject.Pairs[j];
          if Pair.JsonString.Value = 'TableName' then Continue;
          Query.ParamByName(Pair.JsonString.Value).Value := Pair.JsonValue.Value;
        end;

        try
          Query.ExecSQL;
        except
          on E: Exception do
            ShowMessage('⚠️ Erreur d’import dans ' + TableName + ' : ' + E.Message);
        end;

        if Assigned(ProgressBar1) then
        begin
          ProgressBar1.Value := i + 1;
          Application.ProcessMessages;
        end;
      end;

      Query.ExecSQL('PRAGMA foreign_keys = ON;');

      if Assigned(ProgressBar1) then
        ProgressBar1.Visible := False;
//
//      ShowMessage(Format('✅ Importation réussie (%d enregistrements) depuis : %s',
//        [JSONArray.Count, ExtractFileName(FileName)]));

    finally
      Query.Free;
      JSONArray.Free;
    end;

    // --- Suppression du fichier temporaire ---
    if TFile.Exists(TempFile) then
      TFile.Delete(TempFile);

  finally
    OpenDlg.Free;
  end;
end;


// -------------------------------------------------------------------
procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False; // on bloque la fermeture jusqu'à la réponse
  TDialogService.MessageDialog(
    'Voulez-vous vraiment quitter l''application ?',
    TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
    TMsgDlgBtn.mbNo,
    0,
    procedure(const AResult: TModalResult)
    begin
      if AResult = mrYes then
        Application.Terminate;  // on quitte vraiment
    end);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  ColorAnim: TColorAnimation;
  MoveAnim: TFloatAnimation;
  NbMembres: Integer;
begin
  InitialiserChart;
  ProgressBar1.Visible:=false;
//  InitialiserBDD;
//  Security;
  var Bg := TRectangle.Create(Self);
  Bg.Parent := Self;
//  Bg.SetBounds(Edit1.Position.X - 2, Edit1.Position.Y - 2, Edit1.Width + 4, Edit1.Height + 4);
  Bg.Fill.Color := TAlphaColorRec.White;    // couleur de fond
  Bg.Stroke.Color := TAlphaColorRec.Gray;   // couleur de bordure
  Bg.XRadius := 12;                         // arrondi horizontal
  Bg.YRadius := 12;                         // arrondi vertical
  Bg.SendToBack;                            // placer derrière l’edit

  Label2.Text:=Uppercase('Membre inscrit');

  Label1.TextAlign := TTextAlign.Center;
  Label1.Font.Style := [TFontStyle.fsUnderline];

  ColorAnim := TColorAnimation.Create(Self);
  ColorAnim.Parent := Label1;
  ColorAnim.PropertyName := 'TextSettings.FontColor';
  ColorAnim.StartValue := TAlphaColorRec.Red;
  ColorAnim.StopValue := TAlphaColorRec.Blue;
  ColorAnim.Duration := 1.5;
  ColorAnim.Loop := True;
  ColorAnim.AutoReverse := True;
  ColorAnim.Enabled := True;

  MoveAnim := TFloatAnimation.Create(Self);
  MoveAnim.Parent := Label1;
  MoveAnim.PropertyName := 'Position.X';
  MoveAnim.StartValue := 0;
  MoveAnim.StopValue := Self.ClientWidth - Label1.Width;
  MoveAnim.Duration := 10;
  MoveAnim.Loop := True;
  MoveAnim.AutoReverse := True;
  MoveAnim.Enabled := True;

//  InitialiserBDD;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  NbMembres, NbUser,NbReunion, NbPunis, NbAmande: Integer;
  CheminDB: string;
begin

  // Centre le formulaire
  Position := TFormPosition.ScreenCenter;

  Rectangle1.Fill.Color := TAlphaColorRec.Darkgrey;
  Rectangle2.Fill.Color := TAlphaColorRec.Royalblue;
  Rectangle3.Fill.Color := TAlphaColorRec.Green;
  Rectangle4.Fill.Color := TAlphaColorRec.Red;
  Rectangle5.Fill.Color := TAlphaColorRec.Darkviolet;
  Rectangle6.Fill.Color := TAlphaColorRec.Yellowgreen;
  button2.Text:='👤 &Membre';
  button3.Text:='📅 &Réunion';
  button4.Text:='💰 S&ocial';
  button5.Text:='📥 Importer';
  button6.Text:='📤 Exporter';
  button7.Text:='⚙ Paramètres';
  button1.Text:='🛡Utilisateur';
  Label4.Text:='REUNION';
  Label9.Text:='MEMBRE PUNIS';
  Label7.Text:='SOCIAL';
  InitialiserBDD;
  Caption:='GESTION DE GROUPE (Menu Principal)';
  Button1.Text:='Utilisateur';
  InitialiserBDD;
  Label6.Text := 'Copyright ' + Chr($00A9) + ' 2025 By Z-Info';
     InitialiserBDD;
     RemplirComboDates;
     CalculerStats;
//  Nombre de membre enregistrer
  try
    with DataModule1.FDQuery1 do
    begin
      Close;
      SQL.Text := 'SELECT COUNT(*) AS Mbtotal FROM t_membre';
      Open;
      NbMembres := FieldByName('Mbtotal').AsInteger;
      Close;
    end;

//    try
//      ComboBox1.Items.Clear;
//      with DataModule1.FDQuery1 do
//          begin
//            Close;
//            SQL.Text := 'SELECT DISTINCT date_reunion FROM t_reunion ORDER BY date_reunion DESC';
//            Open;
//            while not EOF do
//            begin
//              ComboBox1.Items.Add(FieldByName('date_reunion').AsString);
//              Next;
//            end;
//            Close;
//          end;
//
//          if ComboBox1.Items.Count > 0 then
//            ComboBox1.ItemIndex := -1; // Sélectionne la plus récente
//        except
//          on E: Exception do
//            ComboBox1.Items.Add('Erreur : ' + E.Message);
//    end;
    Label3.Text :=NbMembres.ToString;
//    Label5.Text :=NbMembres.ToString;
//    Label10.Text :=NbMembres.ToString;
    Label8.Text :=NbMembres.ToString;
  except
    on E: Exception do
      Label3.Text := 'Erreur : ' + E.Message;
  end;
  //  Nombre de reunion enregistrer
  try
    with DataModule1.FDQuery1 do
    begin
      Close;
      SQL.Text := 'SELECT COUNT(*) AS NBtotal FROM t_reunion';
      Open;
      NbReunion := FieldByName('NBtotal').AsInteger;
      Close;
    end;
    Label5.Text :=NbReunion.ToString;
  except
    on E: Exception do
      Label3.Text := 'Erreur : ' + E.Message;
  end;

//  Nombre d'utilisateur enregistrer
  try
    with DataModule1.FDQuery1 do
    begin
      Close;
      SQL.Text := 'SELECT COUNT(*) AS NBtotal FROM t_utilisateur';
      Open;
      NbUser := FieldByName('NBtotal').AsInteger;
      Close;
    end;
    Label11.Text:='UTILISATEUR INSCRIT';
    Label12.Text :=NbUser.ToString;
  except
    on E: Exception do
      Label3.Text := 'Erreur : ' + E.Message;
  end;
//  ChargerStatistiques; // ta procédure qui met à jour Label5 et Label10
  CalculerStats; // ensuite on calcule les %
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
end;

// === FILTRAGE PAR DATE DANS COMBOBOX ===
procedure TForm1.Button1Click(Sender: TObject);
begin
    Form4.Show;  // Afficher Form2
    Form1.Hide;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
    if not Assigned(Form2) then
    Form2 := TForm2.Create(Self);
    Form2.Left   := Form1.Left;
    Form2.Top    := Form1.Top;
    Form2.Width  := Form1.Width;
    Form2.Height := Form1.Height;
//    ShowSplash;
    Form2.Show;
    Form1.Hide;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if not Assigned(Form6) then
    Form6 := TForm6.Create(Self);
    Form6.Left   := Form1.Left;
  Form6.Top    := Form1.Top;
  Form6.Width  := Form1.Width;
  Form6.Height := Form1.Height;
//  ShowSplash;
  Form6.Show;  // Afficher Form6
  Form1.Hide;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if not Assigned(Form7) then
    Form7 := TForm7.Create(Self);
    Form7.Left   := Form1.Left;
  Form7.Top    := Form1.Top;
  Form7.Width  := Form1.Width;
  Form7.Height := Form1.Height;
//  ShowSplash;
  Form7.Show;  // Afficher Form
  Form1.Hide;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  ImporterTables;
  FormShow(Self);
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  ExporterTables;
  FormShow(Self);
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  Form8.ShowModal;
end;

procedure TForm1.Rectangle2Click(Sender: TObject);
begin
  Button2Click(Button2);
end;

procedure TForm1.Rectangle3Click(Sender: TObject);
begin
  Button3Click(Button3);
end;

procedure TForm1.Rectangle4Click(Sender: TObject);
begin
    Button4Click(Button4);
end;

procedure TForm1.Rectangle6Click(Sender: TObject);
begin
  Button1Click(Button1);;
end;

end.

