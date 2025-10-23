-- ============================================
--  Base de données : bdd.db
--  Auteur : RASIVIANDRIANARISON Lahiniriko Mahaleo (ZrNarison)
--  Description : Structure pour gestion de groupe multi-périphérique
--  Version : 1.0
-- ============================================

PRAGMA foreign_keys = ON;

-- TABLE MEMBRE
-- Table membre
CREATE TABLE IF NOT EXISTS membre (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    matricule INTEGER UNIQUE,
    nom TEXT NOT NULL,
    prenom TEXT NOT NULL,
    date_naissance DATE,
    lieu_naissance TEXT,
    filliation_pere TEXT,
    filliation_mere TEXT,
    sexe TEXT,
    cin TEXT,
    adresse TEXT,
    email TEXT,
    telephone TEXT,
    image BLOB,
    droit_inscription REAL DEFAULT 35000
);

-- Table reunion
CREATE TABLE IF NOT EXISTS reunion (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    matricule INTEGER,
    date_reunion DATE,
    valajouter REAL,
    FOREIGN KEY (matricule) REFERENCES membre(matricule)
);

-- Table depense
CREATE TABLE IF NOT EXISTS depense (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    date_depense DATE,
    motif TEXT
);

-- Table punition
CREATE TABLE IF NOT EXISTS punition (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    matricule INTEGER,
    date_reunion DATE,
    motif TEXT,
    FOREIGN KEY (matricule) REFERENCES membre(matricule)
);

-- Table pret
CREATE TABLE IF NOT EXISTS pret (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    matricule INTEGER,
    date_pret DATE,
    somme REAL,
    FOREIGN KEY (matricule) REFERENCES membre(matricule)
);

-- Table utilisateur
CREATE TABLE IF NOT EXISTS utilisateur (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    cin TEXT UNIQUE,
    categorie TEXT
);
