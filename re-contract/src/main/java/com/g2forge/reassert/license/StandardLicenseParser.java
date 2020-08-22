package com.g2forge.reassert.license;

import java.util.regex.Pattern;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.api.licenseparser.ILicenseParser;
import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.core.model.contract.license.UnknownLicense;
import com.g2forge.reassert.core.model.contract.license.UnspecifiedLicense;

public class StandardLicenseParser implements ILicenseParser, ISingleton {
	protected static final StandardLicenseParser INSTANCE = new StandardLicenseParser();

	public static StandardLicenseParser create() {
		return INSTANCE;
	}

	protected StandardLicenseParser() {}

	@ReassertLegalOpinion
	@Override
	public ILicense parse(String text) {
		if (text == null) return UnspecifiedLicense.create();

		{ // Apache 2.0
			if ("The Apache License, Version 2.0".equals(text)) return StandardLicense.Apache2;
			final boolean multiline = text.indexOf('\n') >= 0;
			if (multiline && Pattern.compile("^\\s+Apache\\s+License\\s+Version\\s+2\\.0").matcher(text).find()) return StandardLicense.Apache2;
		}

		return new UnknownLicense(text);
	}
}
