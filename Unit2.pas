unit Unit2;

interface

uses
  Controls;

type
  TMyControl = class(TCustomControl)
    procedure Paint(); override;
  end;

implementation

{ TMyControl }

procedure TMyControl.Paint;
begin
  inherited;
  Canvas.Rectangle(0, 0, Width, Height);
end;

end.
