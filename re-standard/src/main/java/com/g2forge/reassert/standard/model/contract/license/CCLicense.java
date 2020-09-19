package com.g2forge.reassert.standard.model.contract.license;

import com.g2forge.alexandria.analysis.ISerializableFunction1;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.core.model.contract.license.ILicenseSpecific;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.license.LicenseVersion;
import com.g2forge.reassert.core.model.contract.terms.ITerms;

import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

@Data
@Builder(toBuilder = true)
public class CCLicense implements ILicenseSpecific {
	@RequiredArgsConstructor
	@Getter
	public enum Flag implements IDescriptorElement {
		Attribution(CCLicense::isAttribution, "BY"),
		NonCommercial(CCLicense::isNonCommercial),
		NoDerivatives(CCLicense::isNoDerivatives),
		ShareAlike(CCLicense::isShareAlike);

		protected final ISerializableFunction1<CCLicense, Boolean> accessor;

		protected final String abbreviation;

		private Flag(ISerializableFunction1<CCLicense, Boolean> accessor) {
			this(accessor, null);
		}

		public String getAbbreviation() {
			return abbreviation == null ? name().replaceAll("[a-z0-9_$]", "") : abbreviation;
		}

		public String getText() {
			return name().replaceAll("[A-Z][a-z]+", "$0 ").trim();
		}
	}

	public interface IDescriptorElement {
		public String getAbbreviation();

		public String getText();
	}

	@Getter
	@RequiredArgsConstructor
	public enum Variant implements IDescriptorElement {
		Generic(null),
		Unported(null),
		International(null),
		Universal(null),
		Austria("AT"),
		IGO("IGO"),
		Italy("IT"),
		US("US");

		protected final String abbreviation;

		@Override
		public String getText() {
			return name();
		}
	}

	protected final boolean zero;

	protected final boolean attribution;

	protected final boolean nonCommercial;

	protected final boolean noDerivatives;

	protected final boolean shareAlike;

	protected final LicenseVersion version;

	protected final Variant variant;

	@Getter(lazy = true)
	@EqualsAndHashCode.Exclude
	@ToString.Exclude
	private final String SPDXShortID = computeString("CC", '-', IDescriptorElement::getAbbreviation);

	@Getter(lazy = true)
	@EqualsAndHashCode.Exclude
	@ToString.Exclude
	private final String name = computeString("Creative Commons", ' ', IDescriptorElement::getText) + " license";

	@Getter(lazy = true)
	private final ITerms<ILicenseTerm> terms = StandardLicense.getLoader().getTerms(getShortID());

	public CCLicense(boolean zero, boolean attribution, boolean nonCommercial, boolean noDerivatives, boolean shareAlike, LicenseVersion version, Variant variant) {
		this.zero = zero;
		this.attribution = attribution;
		this.nonCommercial = nonCommercial;
		this.noDerivatives = noDerivatives;
		this.shareAlike = shareAlike;
		this.version = version;
		this.variant = variant;

		if (!isValid()) throw new IllegalArgumentException(toString());
	}

	protected String computeString(final String prefix, final char separator, final IFunction1<IDescriptorElement, String> accessor) {
		final StringBuilder builder = new StringBuilder().append(prefix);
		if (isZero()) {
			builder.append(accessor.apply(new IDescriptorElement() {
				@Override
				public String getAbbreviation() {
					return "0";
				}

				@Override
				public String getText() {
					return " Zero";
				}
			}));
		}

		for (Flag flag : Flag.values()) {
			if (!flag.getAccessor().apply(this)) continue;
			builder.append(separator).append(accessor.apply(flag));
		}

		final LicenseVersion version = getVersion();
		if (version != null) builder.append(separator).append(version);

		final String variant = getVariant() == null ? null : accessor.apply(getVariant());
		if (variant != null) builder.append(separator).append(variant);
		return builder.toString();
	}

	@Override
	public StandardLicenseFamily getFamily() {
		return StandardLicenseFamily.CC;
	}

	protected boolean hasFlags() {
		boolean flags = false;
		for (Flag flag : Flag.values()) {
			flags |= flag.getAccessor().apply(this);
			if (flags) break;
		}
		return flags;
	}

	public boolean isFamily() {
		return !isZero() && !hasFlags();
	}

	@Override
	public boolean isOrLater() {
		return false;
	}

	public boolean isValid() {
		return !(isZero() && hasFlags());
	}

	@Override
	public String toString() {
		return getShortID();
	}
}
