package com.g2forge.reassert.license.v2;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.core.api.licenseparser.ILicenseParser;
import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.core.model.contract.license.UnknownLicense;
import com.g2forge.reassert.license.StandardLicense;

import lombok.AccessLevel;
import lombok.Getter;

public class StandardLicenseParser implements ILicenseParser, ISingleton {
	protected static final StandardLicenseParser INSTANCE = new StandardLicenseParser();

	public static StandardLicenseParser create() {
		return INSTANCE;
	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final Map<StandardLicense, List<Pattern>> patterns = computePatterns();

	protected StandardLicenseParser() {}

	protected Map<StandardLicense, List<Pattern>> computePatterns() {
		final PatternMapBuilder builder = new PatternMapBuilder();
		builder.license(StandardLicense.Apache2).text("Apache").version(2, 0).build();
		{
			final String gpl = "GPL";
			builder.license(StandardLicense.GPL2Only).text(gpl).child(false).version(2, 0).build().build();
			builder.license(StandardLicense.GPL3Only).text(gpl).version(3, 0).build();
			builder.license(StandardLicense.GPL3OrLater).text(gpl).version(3, 0).text("+").build();
		}
		{
			final String lgpl = "LGPL";
			builder.license(StandardLicense.LGPL21Only).text(lgpl).child(false).version(2, 1).build().build();
			builder.license(StandardLicense.LGPL21OrLater).text(lgpl).version(2, 1).text("+").build();
			builder.license(StandardLicense.LGPL3Only).text(lgpl).version(3, 0).build();
			builder.license(StandardLicense.LGPL3OrLater).text(lgpl).version(3, 0).text("+").build();
		}
		return builder.build();
	}

	@Override
	public ILicense parse(String text) {
		final Set<StandardLicense> retVal = new LinkedHashSet<>();
		final Map<StandardLicense, List<Pattern>> patterns = getPatterns();
		for (Map.Entry<StandardLicense, List<Pattern>> entry : patterns.entrySet()) {
			for (Pattern pattern : entry.getValue()) {
				if (pattern.matcher(text.trim()).matches()) {
					retVal.add(entry.getKey());
					break;
				}
			}
		}
		if (retVal.size() == 1) return HCollection.getOne(retVal);
		return new UnknownLicense(text);
	}
}
