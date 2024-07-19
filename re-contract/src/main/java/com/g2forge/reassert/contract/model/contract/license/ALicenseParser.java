package com.g2forge.reassert.contract.model.contract.license;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.function.builder.IBuilder;
import com.g2forge.alexandria.parse.IMatcher;
import com.g2forge.alexandria.parse.IMatcherBuilder;
import com.g2forge.alexandria.parse.NamedCharacterClass;
import com.g2forge.alexandria.parse.regex.Regex;
import com.g2forge.alexandria.parse.regex.RegexMatcher;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.api.parser.IParser;
import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.license.ILicenseFamily;
import com.g2forge.reassert.core.model.contract.license.LicenseVersion;
import com.g2forge.reassert.core.model.contract.license.UnknownLicense;
import com.g2forge.reassert.core.model.contract.license.UnspecifiedLicense;

import lombok.ToString;

public abstract class ALicenseParser implements IParser<ILicenseApplied> {
	@ReassertLegalOpinion
	public static abstract class APatternListBuilder implements IBuilder<List<IMatcher<? extends ILicenseFamily, ?>>> {
		protected static final IMatcher<?, Regex> gap = pattern().charClass(false, cc -> cc.character('-').character('_').named(NamedCharacterClass.Space)).star().build();

		protected static final IMatcher<?, Regex> parenthetical = pattern().group(g -> g.with(gap).text("(").charClass(true, cc -> cc.character(')')).plus().text(")")).opt().build();

		protected static final IMatcher<?, Regex> holder = pattern().group(g0 -> g0.group(g1 -> g1.with(gap).charClass(false, cc -> cc.range('a', 'z').range('A', 'Z').range('0', '9')).plus()).plus()).opt().build();

		protected static final IMatcher<?, Regex> the = pattern().group(g -> g.text("the").with(gap)).opt().build();

		protected static final IMatcher<?, Regex> licenseReq = pattern().with(gap).text("Licen").charClass(false, cc -> cc.character('c').character('s')).text("e").group(g -> g.charClass(false, cc -> cc.character('s').character('d'))).opt().build(), licenseOpt = pattern().group(g -> g.with(licenseReq)).opt().build();

		protected static final IMatcher<?, Regex> publiclicense = pattern().group(g0 -> g0.group(g1 -> g1.with(gap).text("Public")).opt().with(licenseReq)).opt().build();

		protected static final IMatcher<LicenseVersion, Regex> versionPatternMinor = computeVersionPattern(LicenseVersion.Field.MINOR), versionPatternPatch = computeVersionPattern(LicenseVersion.Field.PATCH);

		protected static IMatcher<LicenseVersion, Regex> computeVersionPattern(LicenseVersion.Field field) {
			if (field == null) throw new NullPointerException();

			final IMatcher<?, Regex> separator = pattern().charClass(false, cc -> cc.character('-').character('.')).build();

			final IMatcherBuilder<LicenseVersion, Regex> builder = RegexMatcher.<LicenseVersion>builder();
			builder.text(",").opt().with(gap).alt(pattern().text("v").text(" ").opt().build(), pattern().text("ve").text("r").opt().text("sion").with(gap).build()).opt();
			builder.group(LicenseVersion::getMajor, g -> g.digit(10).plus());
			if (LicenseVersion.Field.MINOR.compareTo(field) <= 0) {
				builder.group(g0 -> {
					g0.with(separator).group(LicenseVersion::getMinor, g1 -> g1.digit(10).plus());
					if (LicenseVersion.Field.PATCH.compareTo(field) <= 0) {
						g0.group(g2 -> g2.with(separator).group(LicenseVersion::getPatch, g3 -> g3.digit(10).plus())).opt();
					}
				}).opt();
			}
			builder.with(separator).opt();

			return builder.buildReq(match -> {
				final String majorString = match.getAsString(LicenseVersion::getMajor);
				if (majorString == null) return null;
				final int major = Integer.parseInt(majorString);

				final Integer minor;
				if (LicenseVersion.Field.MINOR.compareTo(field) <= 0) {
					final String minorString = match.getAsString(LicenseVersion::getMinor);
					minor = minorString == null ? 0 : Integer.parseInt(minorString);
				} else minor = null;

				final Integer patch;
				if (LicenseVersion.Field.PATCH.compareTo(field) <= 0) {
					final String patchString = match.getAsString(LicenseVersion::getPatch);
					patch = patchString == null ? 0 : Integer.parseInt(patchString);
				} else patch = null;

				return new LicenseVersion(major, minor, patch);
			});
		}

		protected static <Parsed> IMatcherBuilder<Parsed, Regex> pattern() {
			return RegexMatcher.<Parsed>builder(RegexMatcher.Flag.CASE_INSENSITIVE);
		}

		protected static <T> IMatcher<T, Regex> pattern(String text) {
			return RegexMatcher.create(gap, text, RegexMatcher.Flag.CASE_INSENSITIVE);
		}
	}

	@ToString(callSuper = true)
	protected static class UnknownLicenseWithCandidates extends UnknownLicense {
		protected final Set<ILicenseFamily> candidates;

		protected UnknownLicenseWithCandidates(String text, Set<ILicenseFamily> retVal) {
			super(text);
			this.candidates = retVal;
		}
	}

	protected abstract List<IMatcher<? extends ILicenseFamily, ?>> getMatchers();

	@Override
	public ILicenseApplied parse(String text) {
		if (text == null) return UnspecifiedLicense.create();

		final String trimmed = text.trim();
		final Set<ILicenseFamily> results = new LinkedHashSet<>();
		for (IMatcher<? extends ILicenseFamily, ?> matcher : getMatchers()) {
			final IOptional<? extends ILicenseFamily> match = matcher.match(trimmed);
			if (!match.isEmpty()) results.add(match.get());
		}
		if (results.size() == 1) {
			final ILicenseFamily retVal = HCollection.getOne(results);
			final ILicenseFamily family = retVal.getFamily();
			if (family != null) family.validate(retVal).throwIfInvalid();
			return retVal;
		}
		return results.isEmpty() ? new UnknownLicense(text) : new UnknownLicenseWithCandidates(text, results);
	}
}
