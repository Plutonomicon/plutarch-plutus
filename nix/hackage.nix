{ pkgs }:
{
  plutarch = {
    "1.4.0" = {
      timestamp = "2024-01-16T11:00:00Z";
      src = pkgs.fetchFromGitHub {
        owner = "plutonomicon";
        repo = "plutarch-plutus";
        rev = "8d6ca9e5ec8425c2f52faf59a4737b4fd96fb01b";
        hash = "sha256-dMdJxXiBJV7XSInGeSR90/sTWHTxBm3DLaCzpN1SER0=";
      };
    };
    "1.5.0" = {
      timestamp = "2024-01-16T11:00:00Z";
      src = pkgs.fetchFromGitHub {
        owner = "plutonomicon";
        repo = "plutarch-plutus";
        rev = "3ad180895aba3e24b5e1909d8b185f7286356f75";
        hash = "sha256-CIUbOt1uSz8MgdcuGce/AoTSA1BRKWlqrxhNPFUayj4=";
      };
    };
    "1.8.1" = {
      timestamp = "2024-07-29T00:00:00Z";
      src = pkgs.fetchFromGitHub {
        owner = "plutonomicon";
        repo = "plutarch-plutus";
        rev = "e50661e24670974b398be19426617bc6389fdac6";
        hash = "sha256-BcqNHF4LCHwGs+Q+nPKNDOAPZwPvBhKDb7f3s/kkFho=";
      };
    };
    "1.10.0" = {
      timestamp = "2024-01-29T00:00:00Z";
      src = pkgs.fetchFromGitHub {
        owner = "plutonomicon";
        repo = "plutarch-plutus";
        rev = "811c6f26b9c7a677cd38f960b9dc4d26971bee57";
        hash = "sha256-cCW7LMXm4Iv541E1Djn4uawbDKKfo9TbcFtc1g083Os=";
      };
    };
    "1.10.1" = {
      timestamp = "2024-01-29T00:00:00Z";
      src = pkgs.fetchFromGitHub {
        owner = "plutonomicon";
        repo = "plutarch-plutus";
        rev = "705375118dc363c6a1b4c2372887a967a7298476";
        hash = "sha256-WSESLdKZttm2EBNv5SY6sZXT+ks87rY3y0gatf1m3Lo=";
      };
    };

    # Unstable
    "0" = {
      timestamp = "2024-10-09T22:38:57Z";
      overrideVersion = true;
      src = ../.;
    };
  };
  plutarch-ledger-api = {
    "3.2.1" = {
      timestamp = "2024-07-29T00:00:00Z";
      subdir = "plutarch-ledger-api";
      src = pkgs.fetchFromGitHub {
        owner = "plutonomicon";
        repo = "plutarch-plutus";
        rev = "e50661e24670974b398be19426617bc6389fdac6";
        hash = "sha256-BcqNHF4LCHwGs+Q+nPKNDOAPZwPvBhKDb7f3s/kkFho=";
      };
    };
    "3.3.0" = {
      timestamp = "2024-01-29T00:00:00Z";
      subdir = "plutarch-ledger-api";
      src = pkgs.fetchFromGitHub {
        owner = "plutonomicon";
        repo = "plutarch-plutus";
        rev = "811c6f26b9c7a677cd38f960b9dc4d26971bee57";
        hash = "sha256-cCW7LMXm4Iv541E1Djn4uawbDKKfo9TbcFtc1g083Os=";
      };
    };

    # Unstable
    "0" = {
      timestamp = "2024-10-09T22:38:57Z";
      overrideVersion = true;
      subdir = "plutarch-ledger-api";
      src = ../.;
    };
  };
  plutarch-orphanage = {
    "1.0.3" = {
      timestamp = "2024-07-29T00:00:00Z";
      subdir = "plutarch-orphanage";
      src = pkgs.fetchFromGitHub {
        owner = "plutonomicon";
        repo = "plutarch-plutus";
        rev = "e50661e24670974b398be19426617bc6389fdac6";
        hash = "sha256-BcqNHF4LCHwGs+Q+nPKNDOAPZwPvBhKDb7f3s/kkFho=";
      };
    };
    "1.1.0" = {
      timestamp = "2024-01-29T00:00:00Z";
      subdir = "plutarch-orphanage";
      src = pkgs.fetchFromGitHub {
        owner = "plutonomicon";
        repo = "plutarch-plutus";
        rev = "811c6f26b9c7a677cd38f960b9dc4d26971bee57";
        hash = "sha256-cCW7LMXm4Iv541E1Djn4uawbDKKfo9TbcFtc1g083Os=";
      };
    };

    # Unstable
    "0" = {
      timestamp = "2024-10-09T22:38:57Z";
      overrideVersion = true;
      subdir = "plutarch-orphanage";
      src = ../.;
    };
  };
}
