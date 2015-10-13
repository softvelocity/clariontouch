  MEMBER('MPlayer.clw')

  PRAGMA('define(init_priority=>3)')

  MAP
    MODULE('MPlayer_BC0.CLW')
MPlayer_BC0:DctInit             PROCEDURE
MPlayer_BC0:DctKill             PROCEDURE
MPlayer_BC0:FilesInit           PROCEDURE
    END
  END

DctInit PROCEDURE
  CODE
  MPlayer_BC0:DctInit
  MPlayer_BC0:FilesInit


DctKill PROCEDURE
  CODE
  MPlayer_BC0:DctKill

