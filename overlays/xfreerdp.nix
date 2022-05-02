self: super:
{
  freerdp = super.freerdpUnstable.overrideAttrs (old: { buildInputs = old.buildInputs ++ (with self; [ libva-full libva-utils ]);});
}
