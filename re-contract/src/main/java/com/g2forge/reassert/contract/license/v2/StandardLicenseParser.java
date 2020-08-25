package com.g2forge.reassert.contract.license.v2;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.function.IConsumer1;
import com.g2forge.reassert.contract.license.StandardLicense;
import com.g2forge.reassert.core.api.licenseparser.ILicenseParser;
import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.core.model.contract.license.UnknownLicense;

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
		{
			builder.license(StandardLicense.Apache2).text("Apache").optional().optional().text("Software").build().text("License").build().optional().version(2, 0).build().build();
		}
		{
			final IConsumer1<IPatternBuilder<?>> suffix = pattern -> pattern.optional().text(".").build().optional().text("Clause").child(false, false).text("s").build().build();
			builder.license(StandardLicense.BSD2).text("BSD").text("2").with(suffix).build();
			builder.license(StandardLicense.BSD3).text("BSD").text("3").with(suffix).build();
		}
		{
			final IConsumer1<IPatternBuilder<?>> license = pattern -> pattern.optional().text("license").build();
			final IConsumer1<IPatternBuilder<?>> only = pattern -> pattern.optional().text("only").build();
			@SuppressWarnings("unchecked")
			final IConsumer1<IPatternBuilder<?>> orlater = pattern -> pattern.alt(p -> p.text("+"), p -> p.text("or").text("later"));
			{
				final String gpl = "GPL";
				builder.license(StandardLicense.GPL2Only).text(gpl).optional().version(2, 0).build().with(only).with(license).build();
				builder.license(StandardLicense.GPL2OrLater).text(gpl).optional().version(2, 0).build().with(orlater).with(license).build();
				builder.license(StandardLicense.GPL3Only).text(gpl).version(3, 0).with(only).with(license).build();
				builder.license(StandardLicense.GPL3OrLater).text(gpl).version(3, 0).with(orlater).with(license).build();
			}
			{
				final String lgpl = "LGPL";
				builder.license(StandardLicense.LGPL2Only).text(lgpl).optional().version(2, 0).build().with(only).with(license).build();
				builder.license(StandardLicense.LGPL2OrLater).text(lgpl).version(2, 0).with(orlater).with(license).build();
				builder.license(StandardLicense.LGPL21Only).text(lgpl).optional().version(2, 1).build().with(only).with(license).build();
				builder.license(StandardLicense.LGPL21OrLater).text(lgpl).version(2, 1).with(orlater).with(license).build();
				builder.license(StandardLicense.LGPL3Only).text(lgpl).version(3, 0).with(license).with(only).build();
				builder.license(StandardLicense.LGPL3OrLater).text(lgpl).version(3, 0).with(orlater).with(license).build();
			}
		}
		{
			builder.license(StandardLicense.ZLIB).text("ZLIB").build();
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
