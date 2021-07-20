/** Punycode & IDN */;

module type CONV = {
  let upoints: string => array(int);
  let ustring: array(int) => string;
};

module Make:
  (CONV: CONV) =>
   {
    exception Bad_input;
    exception Overflow;

    /** {1 punycode conversion} */;

    let encode: string => string;
    let decode: string => string;

    /** {1 IDN conversion} */;

    let encode_domain: string => string;
    let decode_domain: string => string;

    let self_test: unit => unit;
  };

/*

 module CONV_Netconversion =
 struct
   let upoints s = Netconversion.uarray_of_ustring `Enc_utf8 s
   let ustring a = Netconversion.ustring_of_uarray `Enc_utf8 a
 end

 module CONV_Camomile =
 struct
   open CamomileLibraryDefault
   let upoints s = Array.init (Camomile.UTF8.length s) (fun i -> Camomile.UChar.uint_code (Camomile.UTF8.get s i))
   let ustring a = Camomile.UTF8.init (Array.length a) (fun i -> Camomile.UChar.chr_of_uint a.(i))
 end

 */
