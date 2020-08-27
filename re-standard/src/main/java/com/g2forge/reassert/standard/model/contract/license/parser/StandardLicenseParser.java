package com.g2forge.reassert.standard.model.contract.license.parser;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.function.IConsumer1;
import com.g2forge.alexandria.java.function.IConsumer3;
import com.g2forge.alexandria.java.function.IPredicate1;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.api.licenseparser.ILicenseParser;
import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.core.model.contract.license.UnknownLicense;
import com.g2forge.reassert.core.model.contract.license.UnspecifiedLicense;
import com.g2forge.reassert.standard.model.contract.license.StandardLicense;

import lombok.AccessLevel;
import lombok.Getter;

public class StandardLicenseParser implements ILicenseParser, ISingleton {
	protected static final StandardLicenseParser INSTANCE = new StandardLicenseParser();

	public static StandardLicenseParser create() {
		return INSTANCE;
	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final Map<StandardLicense, List<IPredicate1<String>>> patterns = computePatterns();

	protected StandardLicenseParser() {}

	@ReassertLegalOpinion
	protected Map<StandardLicense, List<IPredicate1<String>>> computePatterns() {
		final PatternMapBuilder<StandardLicense> builder = new PatternMapBuilder<>();
		final IConsumer1<IPatternBuilder<?>> license = pattern -> pattern.optional().text("license").build();
		final IConsumer1<IPatternBuilder<?>> publiclicense = pattern -> pattern.optional().optional().text("Public").build().text("License").build();
		{
			final IConsumer1<IPatternBuilder<?>> prefix = pattern -> pattern.optional().text("The").build().text("Apache").optional().optional().text("Software").build().text("License").build();
			builder.license(StandardLicense.Apache11).with(prefix).version(1, 1).build();
			// Opinion: default to apache 2.0 when version isn't specified
			builder.license(StandardLicense.Apache2).with(prefix).optional().version(2, 0).build().build();
			builder.license(StandardLicense.Apache2, text -> {
				final boolean multiline = text.indexOf('\n') >= 0;
				return multiline && Pattern.compile("^\\s+Apache\\s+License\\s+Version\\s+2\\.0").matcher(text).find();
			});
		}
		{
			final String artistic = "Artistic";
			builder.license(StandardLicense.Artistic1).text(artistic).version(1, 0).build();
			builder.license(StandardLicense.Artistic2).text(artistic).version(2, 0).build();
		}
		{
			final IConsumer1<IPatternBuilder<?>> suffix = pattern -> pattern.optional().text(".").build().optional().text("Clause").child(false, false).text("s").build().build();
			final String bsd = "BSD";
			for (int i = 1; i < 6; i++) {
				final String string = Integer.toString(i);
				final StandardLicense actual = StandardLicense.valueOf(bsd + string);
				builder.license(actual).text(bsd).text(string).with(suffix).with(license).build();
				builder.license(actual).text(string).with(suffix).text(bsd).with(license).build();
			}
		}
		{
			final String cc = "CC", by = "BY", sa = "SA";
			@SuppressWarnings("unchecked")
			final IConsumer1<IPatternBuilder<?>> suffixes = pattern -> pattern.optional().alt(p -> p.text("Unported"), p -> p.text("IT"), p -> p.text("US")).build();
			builder.license(StandardLicense.CC01).text("CC0").with(license).optional().version(1, 0).build().build();
			builder.license(StandardLicense.CCBY3).text(cc).text(by).version(3, 0).with(suffixes).build();
			builder.license(StandardLicense.CCBYSA2).text(cc).text(by).text(sa).version(2, 0).with(suffixes).build();
			builder.license(StandardLicense.CCBYSA3).text(cc).text(by).text(sa).version(3, 0).with(suffixes).build();
		}
		{
			final String eclipse = "Eclipse", epl = "EPL";
			builder.license(StandardLicense.EPL1).text(eclipse).with(publiclicense).version(1, 0).build();
			builder.license(StandardLicense.EPL1).text(epl).version(1, 0).build();
			builder.license(StandardLicense.EPL2).text(eclipse).with(publiclicense).version(2, 0).build();
			builder.license(StandardLicense.EPL2).text(epl).version(2, 0).build();
		}
		{
			final IConsumer1<IPatternBuilder<?>> only = pattern -> pattern.optional().text("only").build().with(license);
			@SuppressWarnings("unchecked")
			final IConsumer1<IPatternBuilder<?>> orlater = pattern -> pattern.alt(p -> p.text("+"), p -> p.text("or").text("later")).with(license);

			final IConsumer3<String, LicenseVersion, Boolean> creator = (name, version, isOrLater) -> {
				final String enumName = name.toUpperCase() + Integer.toString(version.getMajor()) + (version.getMinor() != 0 ? Integer.toString(version.getMinor()) : "") + (isOrLater ? "OrLater" : "Only");
				final StandardLicense standardLicense = StandardLicense.valueOf(enumName);
				builder.license(standardLicense).text(name).version(version).with(isOrLater ? orlater : only).build();
			};

			for (boolean isOrLater : new boolean[] { false, true }) {
				for (LicenseVersion version : HCollection.asList(new LicenseVersion(1, 0), new LicenseVersion(2, 0), new LicenseVersion(2, 1), new LicenseVersion(3, 0))) {
					creator.accept("GPL", version, isOrLater);
				}
				for (LicenseVersion version : HCollection.asList(new LicenseVersion(2, 0), new LicenseVersion(2, 1), new LicenseVersion(3, 0))) {
					creator.accept("LGPL", version, isOrLater);
				}
				for (LicenseVersion version : HCollection.asList(new LicenseVersion(3, 0))) {
					creator.accept("AGPL", version, isOrLater);
				}
				for (LicenseVersion version : HCollection.asList(new LicenseVersion(1, 1), new LicenseVersion(1, 2), new LicenseVersion(1, 3))) {
					creator.accept("GFDL", version, isOrLater);
				}
			}
		}
		{
			final String mpl = "MPL", mozilla = "Mozilla";
			builder.license(StandardLicense.MPL11).text(mpl).version(1, 1).build();
			builder.license(StandardLicense.MPL11).text(mozilla).with(publiclicense).version(1, 1).build();
			builder.license(StandardLicense.MPL2).text(mpl).version(2, 0).build();
			builder.license(StandardLicense.MPL2).text(mozilla).with(publiclicense).version(2, 0).build();
		}
		{
			builder.license(StandardLicense.OFL11).optional().text("SIL").build().text("OFL").version(1, 1).build();
			builder.license(StandardLicense.OFL11).text("Open").text("Font").text("License").version(1, 1).build();
		}
		{
			builder.license(StandardLicense.AFL21).text("AFL").version(2, 1).build();
			builder.license(StandardLicense.Beerware).text("Beer").text("ware").build();
			builder.license(StandardLicense.BSL1).text("Boost").optional().optional().text("Software").build().text("License").build().optional().version(1, 0).build().build();
			builder.license(StandardLicense.BSL1).text("BSL").optional().version(1, 0).build().build();
			builder.license(StandardLicense.CDDL11).text("CDDL").version(1, 1).build();
			builder.license(StandardLicense.EDL1).text("EDL").version(1, 0).build();
			builder.license(StandardLicense.FTL).text("FTL").build();
			builder.license(StandardLicense.ISC).text("ISC").optional().text("original").build().build();
			builder.license(StandardLicense.MIT).text("MIT").with(license).build();
			builder.license(StandardLicense.MIT).text("Expat").child(false, false).text("/MIT").build().build();
			builder.license(StandardLicense.Perl5).text("Perl").text("5").with(license).optional().text("(").text("GPL").text("or").text("Artistic").text(")").build().build();
			builder.license(StandardLicense.PostgreSQL).text("Postgres").child(false, false).text("ql").build().build();
			builder.license(StandardLicense.PSF2).text("PSF").version(2, 0).build();
			builder.license(StandardLicense.PublicDomain).text("Public").text("Domain").build();
			builder.license(StandardLicense.Python2).text("Python").version(2, 0).build();
			builder.license(StandardLicense.WTFPL).text("WTFPL").build();
			builder.license(StandardLicense.ZLIB).text("ZLIB").with(license).build();
		}
		return builder.build();
	}

	@Override
	public ILicense parse(String text) {
		if (text == null) return UnspecifiedLicense.create();

		final Set<StandardLicense> retVal = new LinkedHashSet<>();
		final Map<StandardLicense, List<IPredicate1<String>>> predicates = getPatterns();
		for (Map.Entry<StandardLicense, List<IPredicate1<String>>> entry : predicates.entrySet()) {
			for (IPredicate1<String> predicate : entry.getValue()) {
				if (predicate.test(text)) {
					retVal.add(entry.getKey());
					break;
				}
			}
		}
		if (retVal.size() == 1) return HCollection.getOne(retVal);
		return new UnknownLicense(text);
	}
}
