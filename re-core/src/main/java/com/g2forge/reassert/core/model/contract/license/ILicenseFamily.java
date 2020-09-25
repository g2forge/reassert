package com.g2forge.reassert.core.model.contract.license;

import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.reassert.core.api.module.Context;
import com.g2forge.reassert.core.model.contract.IContractIdentified;
import com.g2forge.reassert.core.model.contract.terms.ITerms;

/**
 * A specific license or a family of licenses. Generally concrete implementations of this interface will be enumerations of known licenses or families.
 * 
 * Any implementation which does not return {@code null} from {@link #getFamily()} should verify that is indeed a child of the parent family.
 */
public interface ILicenseFamily extends ILicense, IContractIdentified {
	public static ITerms<ILicenseTerm> getTerms(ILicenseFamily license) {
		return Context.getContext().getTermsLoader().getTerms(license, ILicenseTerm.class);
	}

	/**
	 * Get the family to which this license or family belongs. May return {@code null} if this belongs to no higher level family.
	 * 
	 * @return The family to which this license or family belongs.
	 */
	public ILicenseFamily getFamily();

	public default boolean isChild(ILicenseFamily license) {
		ILicenseFamily parent = this;
		while (parent != null) {
			if (parent == license) return true;
			parent = parent.getFamily();
		}
		return false;
	}

	public default IValidation validate(ILicenseFamily child) {
		return new ChildLicenseFamilyValidation(false);
	}
}
