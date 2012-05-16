unit JawsCtrls;

// Jaws Accessibility Controls
//
// Reclass of some windowed controls, so Jaws will recognize them
// For some reason or another DEdit is recognized but TEdit isn't

interface
uses
  Classes, StdCtrls, ComCtrls;

type
  DEdit = class(TEdit);
  DListView = class(TListView);

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('NM G2', [DEdit, DListView]);
end;


end.
