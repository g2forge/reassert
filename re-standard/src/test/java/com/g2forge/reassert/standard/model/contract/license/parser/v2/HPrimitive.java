package com.g2forge.reassert.standard.model.contract.license.parser.v2;

import com.g2forge.alexandria.java.core.marker.Helpers;

import lombok.experimental.UtilityClass;

@Helpers
@UtilityClass
public class HPrimitive {
	public static Integer parseInteger(String string) {
		return (string == null) ? null : Integer.parseInt(string);
	}

	public static Long parseLong(String string) {
		return (string == null) ? null : Long.parseLong(string);
	}

	public static Short parseShort(String string) {
		return (string == null) ? null : Short.parseShort(string);
	}

	public static Byte parseByte(String string) {
		return (string == null) ? null : Byte.parseByte(string);
	}

	public static Double parseDouble(String string) {
		return (string == null) ? null : Double.parseDouble(string);
	}

	public static Float parseFloat(String string) {
		return (string == null) ? null : Float.parseFloat(string);
	}
}
