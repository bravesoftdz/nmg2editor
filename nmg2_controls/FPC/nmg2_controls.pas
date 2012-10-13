{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit nmg2_controls; 

interface

uses
    g2_classes, g2_database, g2_file, g2_graph, g2_types, g2_usb, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('g2_classes', @g2_classes.Register); 
  RegisterUnit('g2_graph', @g2_graph.Register); 
end; 

initialization
  RegisterPackage('nmg2_controls', @Register); 
end.
