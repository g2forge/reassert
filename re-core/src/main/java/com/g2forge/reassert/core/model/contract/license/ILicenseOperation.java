package com.g2forge.reassert.core.model.contract.license;

import java.util.List;

public interface ILicenseOperation extends ILicenseApplied {
	public Object getOperation();

	public List<ILicenseApplied> getArguments();
}
