package com.g2forge.reassert.license.v2;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
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
		final Map<StandardLicense, List<Pattern>> retVal = new HashMap<>();
		final Function<? super StandardLicense, ? extends List<Pattern>> creator = l -> new ArrayList<>();
		retVal.computeIfAbsent(StandardLicense.Apache2, creator).add(new PatternBuilder().text("Apache").separator(false).version(2, 0).build());
		retVal.computeIfAbsent(StandardLicense.GPL2Only, creator).add(new PatternBuilder().text("GPL").optional().separator(false).version(2, 0).build().build());
		retVal.computeIfAbsent(StandardLicense.GPL3Only, creator).add(new PatternBuilder().text("GPL").separator(false).version(3, 0).build());
		retVal.computeIfAbsent(StandardLicense.GPL3OrLater, creator).add(new PatternBuilder().text("GPL").separator(false).version(3, 0).separator(false).text("+").build());
		retVal.computeIfAbsent(StandardLicense.LGPL21Only, creator).add(new PatternBuilder().text("LGPL").optional().separator(false).version(2, 1).build().build());
		retVal.computeIfAbsent(StandardLicense.LGPL3Only, creator).add(new PatternBuilder().text("LGPL").separator(false).version(3, 0).build());
		return retVal;
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
